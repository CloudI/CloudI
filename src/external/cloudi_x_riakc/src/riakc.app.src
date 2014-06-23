{application, riakc,
 [{description, "Riak Client"},
  {vsn, "1.4.1"},
  {modules, [
             riakc_pb_socket,
             riakc_obj
            ]},
  {applications, [
                  kernel,
                  stdlib,
                  riak_pb
                 ]},
  {registered, []},
  {env, [
         %% Set default timeout for operations.
         %% Individual operation timeouts can be supplied,
         %% e.g. get_timeout, put_timeout that will 
         %% override the default.
         {timeout, 60000}
        ]}
 ]}.

