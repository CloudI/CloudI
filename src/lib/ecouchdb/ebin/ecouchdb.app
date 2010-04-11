{application, ecouchdb,
 [{description, "Erlang CouchDB client"},
  {vsn, "0.2.4"},
  {modules, [
       ecouchdb_conn,
       ecouchdb
       ]},
  {registered, []},
  {applications, [kernel, stdlib, mochiweb]}
]}.
