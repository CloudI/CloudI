erlang_couchdb is a really simple CouchDB client. Simple means that it does as little as possible and doesn't get in the way. I developed this module because the existing modules seemed too big and did too much for my taste. This module provides several public functions to do things like manipulating databases, documents and views.

The implemented functionality is really limited because I'm only really implementing the stuff that I'm using in I Play WoW.

* Get server information
* Create database
* Get database information
* Create document
* Create document with specific ID
* Update document
* Get document
* Create design document
* Invoke a design document

A quick demo:

    ecouchdb:create_database("localhost", 5984, "iplaywow", 5000).
    ecouchdb:database_info("localhost", 5984, "iplaywow", 5000).
    ecouchdb:server_info("localhost", 5984, 5000).
    ecouchdb:create_document("localhost", 5984, "iplaywow", [{<<"name">>, <<"Korale">>}, {<<"type">>, <<"character">>}], 5000).
    ecouchdb:retrieve_document("localhost", 5984, "iplaywow", "0980...", 5000).
    ecouchdb:update_document("localhost", 5984, "iplaywow", "0980...", [{<<"_rev">>, <<"3419...">>}, {<<"name">>, <<"Korale">>}, {<<"level">>, <<"70">>}, {<<"type">>}, <<"character">>}], 5000).
    ecouchdb:delete_document("localhost", 5984, "iplaywow", "1fd0...", "1193...", 5000).
    ecouchdb:create_view("localhost", 5984, "iplaywow", "characters", <<"javascript">>, [{<<"realm">>, <<"function(doc) { if (doc.type == 'character')  emit(doc.realm_full, null) }">>}], 5000).
    ecouchdb:invoke_view("localhost", 5984, "iplaywow", "characters", "realm", [{"key", "\"Medivh-US\""}], 5000).

Patches are welcome. For the time being this module should be considered alpha. Support is limited but feel free to contact me via email and submit patches. If you use this module please let me know.

To retrieve object you can do:

    {json, Obj} = ecouchdb:invoke_view(...),
    ecouchdb:get_value(<<"rows">>, Obj),
    ecouchdb:get_value([<<"rows">>,<<"value">>], Obj).

To create an object and set a number of attributes:

    ecouchdb:fold([ecouchdb:set_value(K, V) || {K,V} <- L],
    ecouchdb:empty())

# TODO

 * Document attachments 

Your contributions are welcome.
