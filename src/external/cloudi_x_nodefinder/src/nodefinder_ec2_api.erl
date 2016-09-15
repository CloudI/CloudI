%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:
%%%
%%%------------------------------------------------------------------------
%%% 
%%% Copyright (C) 2010 Brian Buchanan. All rights reserved.
%%% 
%%% Redistribution and use in source and binary forms, with or without
%%% modification, are permitted provided that the following conditions
%%% are met:
%%% 1. Redistributions of source code must retain the above copyright
%%%    notice, this list of conditions and the following disclaimer.
%%% 2. Redistributions in binary form must reproduce the above copyright
%%%    notice, this list of conditions and the following disclaimer in the
%%%    documentation and/or other materials provided with the distribution.
%%% 
%%% THIS SOFTWARE IS PROVIDED BY AUTHOR AND CONTRIBUTORS "AS IS" AND
%%% ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
%%% IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
%%% ARE DISCLAIMED.  IN NO EVENT SHALL AUTHOR OR CONTRIBUTORS BE LIABLE
%%% FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
%%% DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
%%% OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
%%% HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
%%% LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
%%% OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
%%% SUCH DAMAGE.
%%%
%%%------------------------------------------------------------------------

%%%------------------------------------------------------------------------
%%% From erlcloud, in erlcloud_ec2.erl
%%%------------------------------------------------------------------------

-module(nodefinder_ec2_api).

%% Library initialization.
-export([new/3]).

%% EC2 API Functions
-export([%% Instances
         describe_instances/1
        ]).

-import(nodefinder_ec2_api_xml, [get_text/2, get_text/3, get_bool/2, get_list/2, get_time/2]).

% -define(API_VERSION, "2009-11-30").
% -define(NEW_API_VERSION, "2012-10-01").
% -define(NEW_API_VERSION, "2013-10-15").
% -define(NEW_API_VERSION, "2014-02-01").
-define(NEW_API_VERSION, "2015-10-01").
-include("nodefinder_ec2_api.hrl").

-spec new(string(), string(), string()) -> aws_config().
new(AccessKeyID, SecretAccessKey, Host) ->
    #aws_config{access_key_id=AccessKeyID,
                secret_access_key=SecretAccessKey,
                ec2_host=Host}.

-spec describe_instances(aws_config()) ->
    {ok, proplist()} | {error, any()}.
describe_instances(Config)
  when is_record(Config, aws_config) ->
    describe_instances([], Config).

-spec describe_instances([string()], aws_config()) ->
    {ok, proplist()} | {error, any()}.
describe_instances(InstanceIDs, Config)
  when is_list(InstanceIDs),
       is_record(Config, aws_config) ->
    Params = nodefinder_ec2_api_aws:param_list(InstanceIDs, "InstanceId"),
    case ec2_query2(Config, "DescribeInstances", Params, ?NEW_API_VERSION) of
        {ok, Doc} ->
            Reservations = xmerl_xpath:string("/DescribeInstancesResponse/reservationSet/item", Doc),
            {ok, [extract_reservation(Item) || Item <- Reservations]};
        {error, Reason} ->
            {error, Reason}
    end.

extract_reservation(Node) ->
    [{reservation_id, get_text("reservationId", Node)},
     {owner_id, get_text("ownerId", Node)},
     %% {group_set, get_list("groupSet/item/groupId", Node)},
     {instances_set, [extract_instance(Item) || Item <- xmerl_xpath:string("instancesSet/item", Node)]}
    ].

extract_instance(Node) ->
    [{instance_id, get_text("instanceId", Node)},
     {group_set, [extract_group(Item) || Item <- xmerl_xpath:string("groupSet/item", Node)]},
     {image_id, get_text("imageId", Node)},
     {instance_state, [
                       {code, list_to_integer(get_text("instanceState/code", Node, "0"))},
                       {name, get_text("instanceState/name", Node)}
                      ]},
     {private_dns_name, get_text("privateDnsName", Node)},
     {dns_name, get_text("dnsName", Node)},
     {reason, get_text("reason", Node, none)},
     {key_name, get_text("keyName", Node, none)},
     {ami_launch_index, list_to_integer(get_text("amiLaunchIndex", Node, "0"))},
     {product_codes, get_list("productCodes/item/productCode", Node)},
     {instance_type, get_text("instanceType", Node)},
     {launch_time, get_time("launchTime", Node)},
     {placement, [{availability_zone, get_text("placement/availabilityZone", Node)}]},
     {kernel_id, get_text("kernelId", Node)},
     {ramdisk_id, get_text("ramdiskId", Node)},
     {monitoring, [{enabled, get_bool("monitoring/enabled", Node)}, {state, get_text("monitoring/state", Node)}]},
     {subnet_id, get_text("subnetId", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {private_ip_address, get_text("privateIpAddress", Node)},
     {ip_address, get_text("ipAddress", Node)},
     {state_reason, [{code, get_text("stateReason/code", Node)}, {message, get_text("stateReason/message", Node)}]},
     {architecture, get_text("architecture", Node)},
     {root_device_type, get_text("rootDeviceType", Node)},
     {root_device_name, get_text("rootDeviceName", Node)},
     {block_device_mapping, [extract_block_device_mapping_status(Item) || Item <- xmerl_xpath:string("blockDeviceMapping/item", Node)]},
     {instance_lifecycle, get_text("instanceLifecycle", Node, none)},
     {spot_instance_request_id, get_text("spotInstanceRequestId", Node, none)},
     {iam_instance_profile, [
                             {arn, get_text("iamInstanceProfile/arn", Node)},
                             {id, get_text("iamInstanceProfile/id", Node)}
                            ]},
     {tag_set,
      [extract_tag_item(Item)
       || Item <- xmerl_xpath:string("tagSet/item", Node)]},
     {network_interface_set, [extract_network_interface(Item) || Item <- xmerl_xpath:string("networkInterfaceSet/item", Node)]}
    ].

extract_group(Node) ->
     [{group_id, get_text("groupId", Node)},
      {group_name, get_text("groupName", Node)}
     ].

extract_block_device_mapping_status(Node) ->
    [
     {device_name, get_text("deviceName", Node)},
     {volume_id, get_text("ebs/volumeId", Node)},
     {status, get_text("ebs/status", Node)},
     {attach_time, get_time("ebs/attachTime", Node)},
     {delete_on_termination, get_bool("ebs/deleteOnTermination", Node)}
    ].

extract_tag_item(Node) ->
    [
     {key, get_text("key", Node)},
     {value, get_text("value", Node)}
    ].

-spec extract_network_interface(Node::tuple()) -> proplist().
extract_network_interface(Node) ->
    [
     {network_interface_id, get_text("networkInterfaceId", Node)},
     {subnet_id, get_text("subnetId", Node)},
     {vpc_id, get_text("vpcId", Node)},
     {availability_zone, get_text("availabilityZone", Node)},
     {description, get_text("description", Node)},
     {owner_id, get_text("ownerId", Node)},
     {requester_managed, get_bool("requesterManaged", Node)},
     {status, get_text("status", Node)},
     {mac_address, get_text("macAddress", Node)},
     {private_ip_address, get_text("privateIpAddress", Node)},
     {source_dest_check, get_bool("sourceDestCheck", Node)},
     {groups_set, [extract_group(Item) || Item <- xmerl_xpath:string("groupSet/item", Node)]},
     {attachment, extract_attachment(Node)},
     {association, extract_association(Node)},
     {tag_set, [extract_tag_item(Item) || Item <- xmerl_xpath:string("tagSet/item", Node)]},
     {private_ip_addresses_set,
            [extract_private_ip_address(Item) || Item <- xmerl_xpath:string("privateIpAddressesSet/item", Node)]}
    ].

-spec extract_attachment(Node::tuple()) -> proplist().
extract_attachment(Node) ->
    [
     {attachment_id, get_text("attachment/attachmentId", Node)},
     {instance_id, get_text("attachment/instanceId", Node)},
     {instance_owner_id, get_text("attachment/instanceOwnerId", Node)},
     {device_index, get_text("attachment/deviceIndex", Node)},
     {status, get_text("attachment/status", Node)},
     {attach_time, get_time("attachment/attachTime", Node)},
     {delete_on_termination, get_bool("attachment/deleteOnTermination", Node)}
    ].

-spec extract_private_ip_address(Node::tuple()) -> proplist().
extract_private_ip_address(Node) ->
    [
     {private_ip_address, get_text("privateIpAddress", Node)},
     {primary, get_bool("primary", Node)}
    ].

-spec extract_association(Node::tuple()) -> proplist().
extract_association(Node) ->
    [
     {public_ip, get_text("association/publicIp", Node)},
     {public_dns_name, get_text("association/publicDnsName", Node)},
     {ip_owner_id, get_text("association/ipOwnerId", Node)},
     {allocation_id, get_text("association/allocationId", Node)},
     {association_id, get_text("association/associationId", Node)}
    ].

ec2_query2(Config, Action, Params, ApiVersion) ->
    QParams = [{"Action", Action}, {"Version", ApiVersion}|Params],
    nodefinder_ec2_api_aws:aws_request_xml4(post, Config#aws_config.ec2_host,
                                            "/", QParams, "ec2", Config).

