%-*-Mode:erlang;coding:utf-8;tab-width:4;c-basic-offset:4;indent-tabs-mode:()-*-
% ex: set ft=erlang fenc=utf-8 sts=4 ts=4 sw=4 et nomod:

% definitions copied from cloudi_core_i_services_monitor.erl
-record(service,
    {
        service_m :: cloudi_core_i_spawn,
        service_f :: start_internal | start_external,
        service_a :: list(),
        process_index :: non_neg_integer(),
        count_process :: pos_integer(), % count_process_dynamic updates this
        count_thread :: pos_integer(),
        scope :: atom(),
        pids :: list(pid()),
        monitor :: reference(),
        restart_count :: non_neg_integer(),
        restart_times :: list(integer()),
        timeout_term :: cloudi_service_api:timeout_terminate_milliseconds(),
        max_r :: non_neg_integer(),
        max_t :: non_neg_integer()
    }).

% definitions copied from cloudi_core_i_constants.hrl
-define(SERVICE_ID_PDICT_KEY, cloudi_service).
-define(SERVICE_FILE_PDICT_KEY, cloudi_service_file).
-define(SCOPE_DEFAULT, cpg_default_scope).
-define(SCOPE_CUSTOM_PREFIX, "cloudi_x_cpg_x_").
-define(SCOPE_FORMAT(Name),
        if
            Name =:= ?SCOPE_DEFAULT ->
                default;
            true ->
                ?SCOPE_CUSTOM_PREFIX ++ L = erlang:atom_to_list(Name),
                erlang:list_to_atom(L)
        end).

