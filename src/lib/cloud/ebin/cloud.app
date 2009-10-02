{application, cloud, 
  [{description, "Cloudi Application"},
   {vsn, "0.0.3"},
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
       cloud_work_interface,
       cloud_work_manager,
       cloud_work_status,
       cloud_work_sup,
       cloud_worker_nodes,
       cloud_worker_scheduler,
       cloud_worker_port_sup,
       cloud_worker_port,
       dynamic_compile,
       immediate_gc,
       monitor_link,
       q,
       lists_extensions,
       math_extensions,
       time_extensions,
       tuple_extensions,
       string_extensions,
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
       rb,
       stdlib,
       kernel
       ]},
   {mod, {cloud_app,[]}},
   {start_phases, []}]}.

