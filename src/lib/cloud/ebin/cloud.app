%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

{application, cloud, 
  [{description, "Cloudi Application"},
   {vsn, "0.0.6"},
   {modules, [
        cloud_api,
        cloud_app,
        cloud_sup,
        cloud_peer,
        cloud_configuration,
        cloud_run_queue,
        cloud_task_speed_lookup,
        cloud_leader,
        cloud_logger,
        cloud_data_repository_sup,
        cloud_work_manager,
        cloud_work_status,
        cloud_work_sup,
        cloud_worker_nodes,
        cloud_worker_scheduler,
        cloud_worker_port_sup,
        cloud_worker_port,
        q,
        'WorkerProtocol'
        ]},
   {registered,[
        cloud_leader,
        cloud_worker_port,     % has dynamic one-based index suffix
        cloud_cworker_port,    % has dynamic one-based index suffix
        cloud_job_,            % has dynamic suffix
        cloud_data_,           % has dynamic suffix
        cloud_worker_port_sup,
        cloud_logger,
        cloud_data_repository_sup,
        cloud_work_manager
        ]},
   {applications, [
        cloud_stdlib,
        cloud_interface,
        rb,
        stdlib,
        kernel
        ]},
   {mod, {cloud_app,[]}},
   {start_phases, []}]}.

