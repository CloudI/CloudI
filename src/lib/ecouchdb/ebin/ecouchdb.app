{application, ecouchdb,
 [{description, "Erlang CouchDB client"},
  {vsn, "0.2.4"},
  {modules, [
       mochiweb_util,
       mochinum,
       mochijson2,
       ecouchdb_conn,
       ecouchdb
       ]},
  {registered, []},
  {applications, [kernel, stdlib]}
]}.
