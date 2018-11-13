-module(connman).

-behavior(gen_server).
-behavior(connman_agent).

%% API
-export([state/0, state/1,
         register_state_notify/2, register_state_notify/3,
         unregister_state_notify/3,
         enable/2, scan/1, technologies/0,
         services/0, service_names/0,
         connect/3]).
%% connman_agent
-export([handle_input_request/2]).
%% Private
-export([get_services/1]).


-type service_descriptor() :: {ebus:object_path(), map()}.
-export_type([service_descriptor/0]).

%% gen_server
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type technology() :: wifi | ethernet | bluetooth.
-type state() :: idle | ready | online | disabled.
-type state_type() :: global | {tech, technology()}.
-type service() :: {ebus:object_path(), map()}.
-export_type([service/0, technology/0, state_type/0, state/0]).

-define(CONNMAN_SERVICE, "net.connman").
-define(CONNMAN_PATH_TECH, "/net/connman/technology").
-define(CONNMAN_PATH_TECH(T), "/net/connman/technology" ++ "/" ++ atom_to_list(T)).

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
                signal_handlers=#{} ::  #{{ebus:object_path(), string()} => {ebus:signal_id(), [pid()]}}
               }).

%%
%% API
%%

-spec get_services(ebus:proxy()) -> {ok, [service_descriptor()]}  | {error, term()}.
get_services(Proxy) ->
    case ebus_proxy:call(Proxy, "net.connman.Manager.GetServices") of
        {ok, [Services]} -> {ok, Services};
        {error, Error} -> {error, Error}
    end.

-spec state() -> state().
state() ->
    state(global).

%% @doc Gets the current state of a given `{tech, Technology}'
%% technology or the "global" state.
-spec state(state_type())-> state().
state(Type) ->
    gen_server:call(?MODULE, {state, Type}).

-spec register_state_notify(Handler::pid(), Info::any())
                           -> {ok, SignalID::ebus:filter_id()} | {error, term()}.
register_state_notify(Handler, Info) ->
    register_state_notify(global, Handler, Info).

-spec register_state_notify(state_type(), Handler::pid(), Info::any())
                           -> {ok, SignalID::ebus:filter_id()} | {error, term()}.
register_state_notify(Type, Handler, Info) ->
    gen_server:call(?MODULE, {register_state_notify, Type, Handler, Info}).

-spec unregister_state_notify(SignalID::ebus:filter_id(), Handler::pid(), Info::any()) -> ok.
unregister_state_notify(SignalID, Handler, Info) ->
    gen_server:call(?MODULE, {unregister_state_notify, SignalID, Handler, Info}).

%% @doc Enable or disable the given `Tech'.
-spec enable(technology(), boolean()) -> ok | {error, term()}.
enable(Tech, Enable) ->
    gen_server:call(?MODULE, {enable, Tech, Enable}).

%% @doc Requests a scan to be started for the given `Tech'. The
%% primary (and currently only) technology that supports scanning is
%% `wifi'.
-spec scan(technology()) -> ok | {error, term()}.
scan(Tech) ->
    gen_server:call(?MODULE, {scan, Tech}).


%% doc Returns the types of currently supported technologies.
-spec technologies() -> [technology()].
technologies() ->
    gen_server:call(?MODULE, technologies).

services() ->
    gen_server:call(?MODULE, services, infinity).

-spec service_names() -> [string()].
service_names() ->
    case services() of
        {ok, Services} ->
            lists:foldl(fun({_, M}, Acc) ->
                                case maps:get("Name", M, false) of
                                    false -> Acc;
                                    Name -> [Name | Acc]
                                end
                        end, [], Services);
        {error, Error} ->
            {error, Error}
    end.

connect(Tech, ServiceName, ServicePass) ->
    gen_server:call(?MODULE, {connect, Tech, ServiceName, ServicePass}, infinity).

handle_input_request(ServicePath, Specs) ->
    gen_server:call(?MODULE, {input_request, ServicePath, Specs}).

%%
%% gen_server
%%

start_link() ->
    {ok, Bus} = ebus:system(),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bus], []).

init([Bus]) ->
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?CONNMAN_SERVICE, []),
    {ok, AgentPid} = connman_agent:start_link(Proxy),
    {ok, #state{bus=Bus, proxy=Proxy, agent=AgentPid}}.


handle_call({enable, Tech, Enable}, _From, State=#state{}) ->
    case tech_is_enabled(Tech, Enable, State)of
        {ok, true} -> {reply, ok, State};
        _ ->
            Reply = ebus_proxy:send(State#state.proxy, ?CONNMAN_PATH_TECH(Tech),
                                    "net.connman.Technology.SetProperty",
                                    [string, variant], ["Powered", Enable]),
            {reply, Reply, State}
    end;
handle_call({state, global}, _From, State=#state{}) ->
    Reply = case ebus_proxy:call(State#state.proxy,
                                 "net.connman.Manager.GetProperties") of
                {ok, [Map]} -> {ok, list_to_atom(maps:get("State", Map))};
                {error, Error} -> {error, Error}
            end,
    {reply, Reply, State};
handle_call({state, {tech, Tech}}, _From, State=#state{}) ->
    Reply = case ebus_proxy:call(State#state.proxy, ?CONNMAN_PATH_TECH(Tech),
                                 "net.connman.Technology.GetProperties") of
                {ok, [Map]} ->
                    case maps:get("Connected", Map) of
                        true -> online;
                        false -> case maps:get("Powered", Map) of
                                     true -> idle;
                                     false -> disabled
                                 end
                    end;
                {error, Error} -> {error, Error}
            end,
    {reply, Reply, State};
handle_call({scan, Tech}, _From, State=#state{}) ->
    Reply = ebus_proxy:send(State#state.proxy, ?CONNMAN_PATH_TECH(Tech),
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

%%
%% register_state_notify
%%

handle_call({register_state_notify, global, Handler, Info}, _From, State=#state{}) ->
    {reply, ebus_proxy:add_signal_handler(State#state.proxy,
                                          "/",
                                          "net.connman.Manager.PropertyChanged",
                                          Handler, Info),
    State};
handle_call({register_state_notify, {tech, Tech}, Handler, Info}, _From, State=#state{}) ->
    {reply, ebus_proxy:add_signal_handler(State#state.proxy,
                                          ?CONNMAN_PATH_TECH(Tech),
                                          "net.connman.Technology.PropertyChanged",
                                          Handler, Info),
     State};
handle_call({register_state_notify, Other, _Handler, _Info}, _From, State=#state{}) ->
    lager:error("Unhandled state notify register: ~p", [Other]),
    {reply, {error, {invalid_state, Other}},
     State};

%%
%% unregister_state_notify

handle_call({unregister_state_notify, SignalID, Handler, Info}, _From, State=#state{}) ->
    {reply, ebus_proxy:remove_signal_handler(State#state.proxy, SignalID, Handler, Info),
    State};

%%
%% input_request
%%

handle_call({input_request, ServicePath, Specs}, _From, State=#state{connects=Connects}) ->
    lager:debug("Looking up input resopnse for ~p", [ServicePath]),
    case lists:keyfind(ServicePath, #connect.service_path, maps:values(Connects)) of
        false ->
            {reply, false, State};
        #connect{service_pass=ServicePass} ->
            {reply, handle_request_input(Specs, ServicePass), State}
    end;

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.


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

%%
%% Private
%%

-spec tech_is_enabled(technology(), boolean(), #state{}) -> {ok, boolean()} | {error, term()}.
tech_is_enabled(Tech, Enabled, State=#state{}) ->
    case ebus_proxy:call(State#state.proxy,
                         ?CONNMAN_PATH_TECH(Tech),
                         "net.connman.Technology.GetProperties") of
        {ok, [Map]} -> {ok, maps:get("Powered", Map) == Enabled};
        {error, Error} -> {error, Error}
    end.

-spec handle_request_input(map(), string()) -> map() | false.
handle_request_input(#{"Passphrase" := #{"Type" := "psk"}}, Pass) ->
    #{"Passphrase" => Pass};
handle_request_input(Specs, _) ->
    lager:error("Unsupported agent input spec: ~p", Specs),
    false.
