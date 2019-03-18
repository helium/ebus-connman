-module(connman_connect).

-behavior(gen_statem).

-record(data, {
               tech :: atom(),
               handler :: pid(),
               proxy :: pid(),
               service_name=undefined :: string() | undefined,
               service_path=undefined :: string() | undefined
              }).

%% gen_statem
-export([callback_mode/0, start/4, start_link/4, init/1,
         searching/3, connecting/3]).

-define(SEARCH_TIMEOUT, 10000).
-define(CONNECT_TIMEOUT, 30000).
-define(CONNECT_RESULT(T, R), {connect_result, T, self(), R}).

callback_mode() -> state_functions.

start(Proxy, Tech, Service, Handler) ->
    gen_statem:start(?MODULE, [Proxy, Tech, Service, Handler], []).

start_link(Proxy, Tech, Service, Handler) ->
    gen_statem:start_link(?MODULE, [Proxy, Tech, Service, Handler], []).

init([Proxy, Tech, Service, Handler]) ->
    Data = #data{proxy=Proxy, tech=Tech, handler=Handler,
                 service_name=connman:service_name(Service),
                 service_path=connman:service_path(Service)},
    case Data#data.service_path of
        undefined ->
            %% Assume that we were at least given a service name
            erlang:send_after(?SEARCH_TIMEOUT, self(), timeout_find_service),
            {ok, searching, Data,
             {next_event, info, find_service}};
        _ ->
            {ok, connecting, Data,
             {next_event, info, connect_service}}
    end.

searching(info, find_service, Data=#data{}) ->
    case connman:service_named(Data#data.tech, Data#data.service_name) of
        not_found ->
            erlang:send_after(1000, self(), find_service),
            keep_state_and_data;
        {Path, _Map} ->
            {next_state, connecting, Data#data{service_path=Path},
             {next_event, info, connect_service}}
    end;
searching(info, timeout_find_service, Data=#data{handler=Handler}) ->
    lager:info("Failed to find SSID ~p", [Data#data.service_name]),
    Handler ! ?CONNECT_RESULT(Data#data.tech, {error, not_found}),
    {stop, normal, Data};
searching(Type, Msg, Data=#data{}) ->
    handle_event(Type, Msg, Data).


connecting(info, connect_service, Data=#data{proxy=Proxy, service_path=Path, handler=Handler, tech=Tech}) ->
    lager:info("Connecting to ~p at ~p", [Data#data.service_name, Path]),
    Handler ! {connect_service, Tech, self(), Path},
    case ebus_proxy:call(Proxy, Path, "net.connman.Service.Connect", [], [], ?CONNECT_TIMEOUT) of
        {error, unknown} ->
            %% Service disappeared while trying to connect. Scan and go back to searching
            connman:scan(Data#data.tech),
            {next_state, searching, Data};
        {error,"net.connman.Error.AlreadyConnected"} ->
            Handler ! ?CONNECT_RESULT(Tech, ok),
            {stop, normal, Data};
        {error,"net.connman.Error.InProgress"} ->
            %% InProgress means that the technology being asked to
            %% connect is already trying to connect to the same _or_ a
            %% different service.
            %%
            %% This can happen when a previous request to
            %% Service.Connect timed out but the underlying connection
            %% attempt was already started. We try to avoid this by
            %% making the CONNECT_TIMEOUT long (as the API docs
            %% suggest).
            %%
            %% The downside of this message is that the original
            %% caller has already received a timeout message so
            %% there's noone really waiting for the original call to
            %% succeed or fail.
            Handler ! ?CONNECT_RESULT(Tech, {error, already_connecting}),
            {stop, normal, Data};
        {ok, []} ->
            Handler ! ?CONNECT_RESULT(Tech, ok),
            {stop, normal, Data};
        Result ->
            Handler ! ?CONNECT_RESULT(Tech, Result),
            {stop, normal, Data}
    end;
connecting(Type, Msg, Data=#data{}) ->
    handle_event(Type, Msg, Data).

%%
%% Internal
%%


handle_event(info, find_service, #data{}) ->
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
