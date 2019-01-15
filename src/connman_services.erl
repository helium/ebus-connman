-module(connman_services).

-behavior(gen_server).
-include("connman.hrl").

-define(SERVICES_SIGNAL_INFO, connman_services).

-record(state, {
                proxy :: bus:proxy(),
                services=[] :: [connman:service_descriptor()],
                services_pid=undefined :: pid() | undefined,
                services_signal_id=-1 :: ebus:signal_id()
               }).

%% gen_server
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
% APIA
-export([services/0, service_names/0, service_named/1, scan/1]).

%% API

-spec scan(connman:technology()) -> ok | {error, term()}.
scan(Tech) ->
    gen_server:call(?MODULE, {scan, Tech}).

-spec services() -> [connman:service_descriptor()].
services() ->
    gen_server:call(?MODULE, services).

-spec service_names() -> [string()].
service_names() ->
    lists:foldl(fun({_, M}, Acc) ->
                        case maps:get("Name", M, false) of
                            false -> Acc;
                            Name -> [Name | Acc]
                        end
                end, [], services()).

-spec service_named(string() | {connman:service_key(), string()}) -> connman:service_descriptor() | not_found.
service_named(Name) when is_list(Name) ->
    service_named({name, Name});
service_named({Key, Name}) ->
    Filter = case Key of
                 path -> fun({Path, _}) ->
                                 Path == Name
                         end;
                 name ->
                     fun({_, M}) ->
                             maps:get("Name", M, false) == Name
                     end
             end,
    case lists:filter(Filter, services()) of
        [] -> not_found;
        [Entry] -> Entry
    end.

start_link(Bus) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bus], []).

init([Bus]) ->
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?CONNMAN_SERVICE, []),
    self() ! init_services,
    {ok, #state{proxy=Proxy}}.


handle_call({scan, Tech}, _From, State=#state{}) ->
    {reply, start_scan(Tech, State), State};
handle_call(services, _From, State=#state{}) ->
    {reply, State#state.services, State};

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.


handle_info(init_services, State=#state{proxy=Proxy}) ->
    Parent = self(),
    ServicesPid = spawn_link(fun() ->
                                     Parent ! {get_services, get_services(Proxy)}
                             end),
    {ok, ServicesSignal} =
        ebus_proxy:add_signal_handler(Proxy, "/", "net.connman.Manager.ServicesChanged",
                                      self(), ?SERVICES_SIGNAL_INFO),
    %% Kick of a scan on wifi to speed up service discovery
    start_scan(wifi, State),
    {noreply, State#state{services_signal_id=ServicesSignal, services_pid=ServicesPid}};
handle_info({get_services, Result}, State=#state{}) ->
    case Result of
        {ok, Services} ->
            NewServices = merge_services(State#state.services, Services),
            {noreply, State#state{services=NewServices, services_pid=undefined}};
        {error, Error} ->
            lager:warning("Failed to fetch services ~p, falling back on signals", [Error]),
            {noreply, State#state{services_pid=undefined}}
    end;
handle_info({ebus_signal, _, SignalID, Msg}, State=#state{services_signal_id=SignalID}) ->
    %% If the service wasn't found before using get_services it may
    %% appear in the changed list as a new service.
    case ebus_message:args(Msg) of
        {ok, [Changed, Removed]} ->
            %% Merge in the changes
            NewServices1 = merge_services(State#state.services, Changed),
            %% And delete any removed
            RemovedSet = sets:from_list(Removed),
            NewServices2 = lists:filter(fun({Path, _}) ->
                                                not sets:is_element(Path, RemovedSet)
                                        end, NewServices1),
            {noreply, State#state{services=NewServices2}};
        {error, Error} ->
            lager:notice("Filter match error: ~p", [Error]),
            {noreply, State}
    end;

handle_info(Msg, State) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.


terminate(_, State=#state{}) ->
    ebus_proxy:remove_signal_handler(State#state.proxy,
                                     State#state.services_signal_id,
                                     self(),
                                     ?SERVICES_SIGNAL_INFO).

%%
%% Internal
%%

start_scan(Tech, State=#state{}) ->
    ebus_proxy:send(State#state.proxy, ?CONNMAN_PATH_TECH(Tech),
                    "net.connman.Technology.Scan").

-spec get_services(ebus:proxy()) -> {ok, [connman:service_descriptor()]}  | {error, term()}.
get_services(Proxy) ->
    case ebus_proxy:call(Proxy, "/", "net.connman.Manager.GetServices", [], [], 15000) of
        {ok, [Services]} -> {ok, Services};
        {error, Error} -> {error, Error}
    end.

-spec merge_services([connman:service_descriptor()], [connman:service_descriptor()])
                    -> [connman:service_descriptor()].
merge_services(CurrentServices, NewServices) ->
    lists:foldl(fun({Path, Map}, Acc)->
                        StoredMap = case lists:keyfind(Path, 1, Acc) of
                                        false -> #{};
                                        {_, M} -> M
                                    end,
                        lists:keystore(Path, 1, Acc, {Path, maps:merge(StoredMap, Map)})
                end, CurrentServices, NewServices).
