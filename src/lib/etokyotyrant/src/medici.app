%% Medici application resource file.
%%
{application, medici,
 [{description, "Medici Tokyo Tyrant interface"},
  {vsn, "0.6"},
  {modules, [
  	     medici,
	     medici_app,
	     medici_sup,
	     medici_controller,
	     medici_native_controller,
	     medici_conn_sup,
	     medici_conn,
	     medici_native_conn,
             medici_port_srv,
             medici_port_sup,
	     principe,
	     principe_table]},
  {registered, []},
  {applications, [kernel, stdlib]},
  {mod, {medici_app, []}},
  {env, [{options, []}]}
 ]}.
