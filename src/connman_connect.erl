-module(connman_connect).

-behavior(gen_statem).

-record(data, {
               tech :: atom(),
               handler :: pid(),
               proxy :: pid(),
               service_name :: string(),
               service_path=undefined :: string() | undefined,
               services_signal_id :: ebus:filter_id()
              }).

%% gen_statem
-export([callback_mode/0, start_link/4, init/1, terminate/2,
         searching/3, connecting/3]).

-define(SEARCH_TIMEOUT, 5000).
-define(CONNECT_RESULT(R), {connect_result, self(), R}).
-define(SERVICES_SIGNAL_INFO, connman_connect).

callback_mode() -> state_functions.

start_link(Proxy, Tech, ServiceName, Handler) ->
    gen_statem:start_link(?MODULE, [Proxy, Tech, ServiceName, Handler], []).

init([Proxy, Tech, ServiceName, Handler]) ->
    {ok, ServicesSignal} =
        ebus_proxy:add_signal_handler(Proxy, "/", "net.connman.Manager.ServicesChanged",
                                      self(), ?SERVICES_SIGNAL_INFO),
    %% Kick of a scan to speed up service discovery
    scan(Tech, Proxy),
    self() ! find_service,
    {ok, searching,
     #data{proxy=Proxy, tech=Tech, handler=Handler,
           service_name=ServiceName, services_signal_id=ServicesSignal},
     {next_event, info, find_service}}.

searching(info, find_service, Data=#data{handler=Handler}) ->
    case connman:get_services(Data#data.proxy) of
        {ok, Services} ->
            erlang:send_after(?SEARCH_TIMEOUT, self(), timeout_find_service),
            {keep_state, Data, {next_event, info, {find_service, Services}}};
        {error, timeout} ->
            erlang:send_after(1000, self(), find_service);
        {error, Error} ->
            Handler ! ?CONNECT_RESULT({error, Error}),
            {stop, normal, Data}

    end;
searching(info, {ebus_signal, _, SignalID, Msg}, Data=#data{services_signal_id=SignalID}) ->
    %% If the service wasn't found before using get_services it may
    %% appear in the changed list as a new service.
    case ebus_message:args(Msg) of
        {ok, [Changed, _]} ->
            {keep_state, Data, {next_event, info, {find_service, Changed}}};
        {error, Error} ->
            lager:notice("Filter match error: ~p", [Error]),
            keep_state_and_data
    end;
searching(info, {find_service, Services}, Data=#data{}) ->
    case find_service(Data#data.service_name, Services) of
        false ->
            keep_state_and_data;
        {Path, _Map} ->
            {next_state, connecting, Data#data{service_path=Path},
             {next_event, info, connect_service}}
    end;
searching(info, timeout_find_service, Data=#data{handler=Handler}) ->
    Handler ! ?CONNECT_RESULT({error, not_found}),
    {stop, normal, Data};
searching(Type, Msg, Data=#data{}) ->
    handle_event(Type, Msg, Data).


connecting(info, connect_service, Data=#data{proxy=Proxy, service_path=Path, handler=Handler}) ->
    lager:debug("Connecting to ~p at ~p", [Data#data.service_name, Path]),
    Handler ! {connect_service, self(), Path},
    case ebus_proxy:call(Proxy, Path, "net.connman.Service.Connect") of
        {error, unknown} ->
            %% Service disappeared while trying to connect. Scan and go back to searching
            scan(Data#data.tech, Proxy),
            {next_state, searching, Data};
        {error,"net.connman.Error.AlreadyConnected"} ->
            Handler ! ?CONNECT_RESULT(ok),
            {stop, normal, Data};
        {ok, []} ->
            Handler ! ?CONNECT_RESULT(ok),
            {stop, normal, Data};
        Result ->
            Handler ! ?CONNECT_RESULT(Result),
            {stop, normal, Data}
    end;
connecting(Type, Msg, Data=#data{}) ->
    handle_event(Type, Msg, Data).

terminate(_, Data=#data{}) ->
    ebus_proxy:remove_signal_handler(Data#data.proxy,
                                     Data#data.services_signal_id,
                                     self(),
                                     ?SERVICES_SIGNAL_INFO).


%%
%% Internal
%%


handle_event(info, {find_service, _}, #data{}) ->
    %% handled only by searching
    keep_state_and_data;
handle_event(info, {ebus_signal, _, _, _}, #data{}) ->
    %% handled only by searching
    keep_state_and_data;
handle_event(info, timeout_find_service, #data{}) ->
    %% handled only by searching
    keep_state_and_data;
handle_event(Type , Msg, #data{}) ->
    lager:warning("Unhandled event ~p: ~p", [Type, Msg]),
    keep_state_and_data.

-spec find_service(string(), list(map())) -> false | connman:service_descriptor().
find_service(Name, Services) ->
    case lists:filter(fun({_Path, Map}) ->
                              maps:get("Name", Map, false) == Name
                      end, Services) of
        [] -> false;
        [Entry] -> Entry
    end.

scan(wifi, Proxy) ->
    ebus_proxy:send(Proxy, "/net/connman/technology/wifi", "net.connman.Technology.Scan"),
    ok;
scan(_, _) ->
    ok.
