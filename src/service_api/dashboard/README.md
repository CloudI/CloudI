Introduction
============

This project provides a web-based dashboard for viewing CloudI environment settings and changing key parameters. It uses the JQuery and DataTables javascript libraries to provide an attractive display with minimal coding needed for different browswer dependencies.

Most screens in the application provide the ability to search for specific text and the ability to change the sort order by clicking on the column name.  In addition, you can specify the host name or address to monitor.  Specific notes for each main screen are listed below.

 
Service Summary
===============
This screen provides a summary of services running on a CloudI node. 

Clicking on a service will display buttons to:

* Remove the service
* Restart the service
* Show subscriptions for that service

This screen refreshes every 60 seconds.

View Log File
=============
This screen displays the current log file contents with a filter for different log levels.  The screen refreshes every 60 seconds.

Note that in order to use this function, there MUST be an entry in the cloudi.conf similar to what is shown below.

        % Used for viewing logs on the Dashboard
        [{type, internal},
         {prefix, "/dashboard/log/"},
         {module, cloudi_service_filesystem},
         {args, [{directory, "logs"},
                {refresh, 60},          % 1 minute
                {cache, 60},            % 1 minute
                {use_http_get_suffix, true}]},      
         {dest_refresh, none},
         {timeout_init, 60000}, 
         {timeout_async, 5000}, 
         {timeout_sync, 5000}, 
         {dest_list_deny, [api]}, 
         {dest_list_allow, undefined}, 
         {count_process, 1}, 
         {max_r, 1}, 
         {max_t, 300}, 
         {options, []}
        ]


Code Path
=========
This screen provides a list of the current code paths and the ability to add a new code path.

Clicking on a code path entry will display buttons to:

* Remove the selected path 

This screen refreshes every 60 seconds.

Nodes 
=====
This screen lists the current node discovery configuration.  It also provides a list of current and previously connected nodes. In addition, a new node can be added or an existing node removed from the cluster.

Log Settings
============
This screen displays the current logging configuration and allows you to change some detailed log settings.

