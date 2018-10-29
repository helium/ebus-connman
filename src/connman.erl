-module(connman).

-behavior(gen_server).
-behavior(connman_agent).

%% Getting the instance
-export([connman/0]).
%% API
-export([state/1, register_state_notify/2, unregister_state_notify/2,
         enable/3, scan/2, technologies/1, services/1,
         connect/4]).
%% connman_agent
-export([handle_input_request/3]).
%% Private
-export([get_services/1]).


-type service_descriptor() :: {ebus:object_path(), map()}.
-export_type([service_descriptor/0]).

%% gen_server
-export([start/0, start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-type service() :: {ebus:object_path(), map()}.
-export_type([service/0]).

-define(CONNMAN_SERVICE, "net.connman").
-define(CONNMAN_PATH_TECH, "/net/connman/technology").

-record(connect, {
                  from :: term(),
                  service_path=undefined :: string() | undefined,
                  service_pass :: string()
                 }).

-record(state, {
                bus :: pid(),
                proxy :: ebus:proxy(),
                agent :: pid(),
                connects=#{} :: #{pid() => #connect{}},
                state_notify_group :: reference(),
                state_signal_id :: ebus:filter_id()
               }).

%%
%% API
%%

connman() ->
    connman_sup:connman().

-spec get_services(ebus:proxy()) -> {ok, [service_descriptor()]}  | {error, term()}.
get_services(Proxy) ->
    case ebus_proxy:call(Proxy, "net.connman.Manager.GetServices") of
        {ok, [Services]} -> {ok, Services};
        {error, Error} -> {error, Error}
    end.

state(Pid) ->
    gen_server:call(Pid, state).

-spec register_state_notify(pid(), Handler::pid()) -> ok.
register_state_notify(Pid, Handler) ->
    gen_server:cast(Pid, {register_state_notify, Handler}).

-spec unregister_state_notify(pid(), Handler::pid()) -> ok.
unregister_state_notify(Pid, Handler) ->
    gen_server:cast(Pid, {unregister_state_notify, Handler}).

enable(Pid, Tech, Enable) ->
    gen_server:call(Pid, {enable, Tech, Enable}).

scan(Pid, Tech) ->
    gen_server:call(Pid, {scan, Tech}).

technologies(Pid) ->
    gen_server:call(Pid, technologies).

services(Pid) ->
    gen_server:call(Pid, services).

connect(Pid, Tech, ServiceName, ServicePass) ->
    gen_server:call(Pid, {connect, Tech, ServiceName, ServicePass}, infinity).

handle_input_request(Pid, ServicePath, Specs) ->
    gen_server:call(Pid, {input_request, ServicePath, Specs}).

%%
%% gen_server
%%

start() ->
    {ok, Bus} = ebus:system(),
    gen_server:start(?MODULE, [Bus], []).

start_link() ->
    {ok, Bus} = ebus:system(),
    gen_server:start_link(?MODULE, [Bus], []).

init([Bus]) ->
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?CONNMAN_SERVICE, []),
    {ok, AgentPid} = connman_agent:start_link(Proxy, ?MODULE, self()),
    {ok, StateSignal} =
        ebus_proxy:add_signal_handler(Proxy, "/", "net.connman.Manager.PropertyChanged", self()),
    GroupID = make_ref(),
    ok = pg2:create(GroupID),
    {ok, #state{bus=Bus, proxy=Proxy, agent=AgentPid,
                state_notify_group=GroupID, state_signal_id=StateSignal}}.


handle_call({enable, Tech, Enable}, _From, State=#state{}) ->
    case tech_is_enabled(Tech, Enable, State)of
        {ok, true} -> {reply, ok, State};
        _ ->
            Reply = ebus_proxy:send(State#state.proxy, tech_path(Tech),
                                    "net.connman.Technology.SetProperty",
                                    [string, variant], ["Powered", Enable]),
            {reply, Reply, State}
    end;
handle_call(state, _From, State=#state{}) ->
    Reply = case ebus_proxy:call(State#state.proxy, "net.connman.Manager.GetProperties") of
                {ok, [Map]} -> {ok, list_to_atom(maps:get("State", Map))};
                {error, Error} -> {error, Error}
            end,
    {reply, Reply, State};
handle_call({scan, Tech}, _From, State=#state{}) ->
    Reply = ebus_proxy:send(State#state.proxy, ?CONNMAN_PATH_TECH ++ "/" ++ atom_to_list(Tech),
                            "net.connman.Technology.Scan"),
    {reply, Reply, State};
handle_call(technologies, _From, State=#state{}) ->
    Reply = case ebus_proxy:call(State#state.proxy, "net.connman.Manager.GetTechnologies") of
                {ok, [Technologies]} ->
                    {ok, [list_to_atom(maps:get("Type", V)) || {_, V} <- Technologies]};
                {error, Error} -> {error, Error}
            end,
    {reply, Reply, State};
handle_call(services, _From, State=#state{}) ->
    {reply, get_services(State#state.proxy), State};

handle_call({connect, Tech, ServiceName, ServicePass}, From, State=#state{}) ->
    lager:debug("Starting connect to ~p", [ServiceName]),
    {ok, ConnectPid} = connman_connect:start_link(State#state.proxy, Tech, ServiceName, self()),
    Connects = maps:put(ConnectPid,
                        #connect{service_pass=ServicePass, from=From},
                        State#state.connects),
    {noreply, State#state{connects=Connects}};
handle_call({input_request, ServicePath, Specs}, _From, State=#state{connects=Connects}) ->
    lager:info("Looking up ~p in ~p", [ServicePath, Connects]),
    case lists:keyfind(ServicePath, #connect.service_path, maps:values(Connects)) of
        false ->
            {reply, false, State};
        #connect{service_pass=ServicePass} ->
            {reply, handle_request_input(Specs, ServicePass), State}
    end;

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.

handle_cast({register_state_notify, Handler}, State=#state{state_notify_group=Group}) ->
    pg2:join(Group, Handler),
    {noreply, State};
handle_cast({unregister_state_notify, Handler}, State=#state{state_notify_group=Group}) ->
    ebus_proxy:remove_signal_handler(State#state.proxy, State#state.state_signal_id),
    pg2:leave(Group, Handler),
    {noreply, State};

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.


handle_info({filter_match, SignalID, Msg}, State=#state{state_signal_id=SignalID}) ->
    case ebus_message:args(Msg) of
        {ok, ["State", NewStateStr]} ->
            NewState = list_to_atom(NewStateStr),
            [Member ! {state_changed, NewState}
             || Member <- pg2:get_members(State#state.state_notify_group)];
        _ -> ok
    end,
    {noreply, State};

handle_info({connect_service, ConnectPid, ServicePath}, State=#state{connects=Connects}) ->
    case maps:get(ConnectPid, Connects, false) of
        false -> {noreply, State};
        C=#connect{} ->
            NewConnects = maps:put(ConnectPid, C#connect{service_path=ServicePath}, Connects),
            {noreply, State#state{connects=NewConnects}}
    end;
handle_info({connect_result, ConnectPid, Result}, State=#state{}) ->
    case maps:take(ConnectPid, State#state.connects) of
        {#connect{from=From}, Connects} ->
            gen_server:reply(From, Result),
            {noreply, State#state{connects=Connects}};
        _ ->
            {noreply, State}
    end;
handle_info(Msg, State=#state{}) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.


terminate(_Reason, State=#state{}) ->
    ebus_proxy:remove_signal_handler(State#state.proxy, State#state.state_signal_id),
    pg2:delete(State#state.state_notify_group).

%%
%% Private
%%

tech_path(Tech) ->
    ?CONNMAN_PATH_TECH ++ "/" ++ atom_to_list(Tech).

-spec tech_is_enabled(atom(), boolean(), #state{}) -> {ok, boolean()} | {error, term()}.
tech_is_enabled(Tech, Enabled, State=#state{}) ->
    case ebus_proxy:call(State#state.proxy, tech_path(Tech), "net.connman.Technology.GetProperties") of
        {ok, [Map]} -> {ok, maps:get("Powered", Map) == Enabled};
        {error, Error} -> {error, Error}
    end.

-spec handle_request_input(map(), string()) -> map() | false.
handle_request_input(#{"Passphrase" := #{"Type" := "psk"}}, Pass) ->
    #{"Passphrase" => Pass};
handle_request_input(Specs, _) ->
    lager:error("Unsupported agent input spec: ~p", Specs),
    false.
