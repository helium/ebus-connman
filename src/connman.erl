-module(connman).

-behavior(gen_server).
-behavior(connman_agent).
-include("connman.hrl").

%% API
-export([state/0, state/1,
         register_state_notify/2, register_state_notify/3,
         unregister_state_notify/3,
         enable/2, scan/1, technologies/0,
         services/0, service_names/0, service_named/1,
         service_name/1, service_path/1,
         connect/4, disconnect/2, start_agent/0]).
%% connman_agent
-export([agent_input_request/2,
         agent_cancel/0,
         agent_retry/1,
         agent_error/2]).

-type service_descriptor() :: {ebus:object_path(), map()}.
-type service_key() :: path | name.
-export_type([service_descriptor/0]).

%% gen_server
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type technology() :: wifi | ethernet | bluetooth.
-type state() :: idle | ready | online | disabled.
-type state_type() :: global | {tech, technology()}.
-type service() :: {ebus:object_path(), map()}.
-export_type([service/0, technology/0, state_type/0, state/0]).

-record(connect, {tech :: technology(),
                  handler :: pid(),
                  monitor :: reference(),
                  pid :: pid(),
                  service_name :: string(),
                  service_path=undefined :: string() | undefined,
                  service_pass :: string()
                 }).

-record(state, {
                proxy :: ebus:proxy(),
                agent=undefined :: pid() | undefined,
                connects=#{} :: #{technology() => #connect{}}
               }).

%%
%% API
%%

-spec state() -> {ok, state()} | {error, term()}.
state() ->
    state(global).

%% @doc Gets the current state of a given `{tech, Technology}'
%% technology or the "global" state.
-spec state(state_type())-> {ok, state()} | {error, term()}.
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
    connman_services:scan(Tech).

%% doc Returns the types of currently supported technologies.
-spec technologies() -> [technology()].
technologies() ->
    gen_server:call(?MODULE, technologies).

-spec services() -> [service_descriptor()].
services() ->
    connman_services:services().

-spec service_names() -> [string()].
service_names() ->
    connman_services:service_names().

-spec service_named(string() | {service_key(), string()}) -> not_found | service_descriptor().
service_named(Name) ->
    connman_services:service_named(Name).

-spec connect(technology(), string() | {service_key(), string()}, string(), pid()) -> ok | {error, term()}.
connect(Tech, Name, ServicePass, Handler) when is_list(Name) ->
    connect(Tech, {name, Name}, ServicePass, Handler);
connect(Tech, Service, ServicePass, Handler) when is_tuple(Service) ->
    gen_server:call(?MODULE, {connect, Tech, Service, ServicePass, Handler}).


-spec service_name({service_key(), string()}) -> string() | undefined.
service_name({name, ServiceName}) ->
    ServiceName;
service_name(_) ->
    undefined.

-spec service_path({service_key(), string()}) -> string() | undefined.
service_path({path, ServicePath}) ->
    ServicePath;
service_path(_) ->
    undefined.

%% @doc Disconnect the named service for a given technology.
-spec disconnect(technology(),  string() | {service_key(), string()}) -> ok | {error, term()}.
disconnect(Tech, Name) when is_list(Name) ->
    disconnect(Tech, {name, Name});
disconnect(Tech, {name, Name}) ->
    case service_named(Name) of
        not_found -> {error, not_found};
        {Path, _} -> disconnect(Tech, {path, Path})
    end;
disconnect(_Tech, {path, Path}) ->
    gen_server:call(?MODULE, {disconnect, Path}).

%% @doc Starts the agent that will respond to agent callbacks from
%% connmand. The agent is not started by default.
-spec start_agent() -> ok | {error, term()}.
start_agent() ->
    gen_server:call(?MODULE, start_agent).

agent_input_request(ServicePath, Specs) ->
    gen_server:call(?MODULE, {agent_input_request, ServicePath, Specs}).

agent_cancel() ->
    gen_server:cast(?MODULE, agent_cancel).

agent_retry(ServicePath) ->
    gen_server:cast(?MODULE, {agent_retry, ServicePath}).

agent_error(ServicePath, Error) ->
    gen_server:cast(?MODULE, {agent_error, ServicePath, Error}).



%%
%% gen_server
%%

start_link(Bus) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Bus], []).

init([Bus]) ->
    {ok, Proxy} = ebus_proxy:start_link(Bus, ?CONNMAN_SERVICE, []),
    {ok, #state{proxy=Proxy}}.

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
                        true -> {ok, online};
                        false -> case maps:get("Powered", Map) of
                                     true -> {ok, idle};
                                     false -> {ok, disabled}
                                 end
                    end;
                {error, Error} -> {error, Error}
            end,
    {reply, Reply, State};
handle_call(technologies, _From, State=#state{}) ->
    Reply = case ebus_proxy:call(State#state.proxy, "net.connman.Manager.GetTechnologies") of
                {ok, [Technologies]} ->
                    {ok, [list_to_atom(maps:get("Type", V)) || {_, V} <- Technologies]};
                {error, Error} -> {error, Error}
            end,
    {reply, Reply, State};

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

handle_call({agent_input_request, ServicePath, Specs}, _From, State=#state{connects=Connects}) ->
    lager:debug("Looking up input resopnse for ~p", [ServicePath]),
    case lists:keyfind(ServicePath, #connect.service_path, maps:values(Connects)) of
        false ->
            {reply, false, State};
        #connect{service_pass=ServicePass} ->
            {reply, handle_request_input(Specs, ServicePass), State}
    end;

handle_call({disconnect, ServicePath}, _From, State=#state{}) ->
    case ebus_proxy:call(State#state.proxy, ServicePath, "net.connman.Service.Disconnect") of
        {ok, []} ->
            {reply, ok, State};
        {error, Error} ->
            {reply, {error, Error}, State}
    end;

handle_call({connect, Tech, Service, ServicePass, Handler}, _From, State=#state{}) ->
    case start_connect(Tech, Service, ServicePass, Handler, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        {Error, NewState} ->
            {reply, Error, NewState}
    end;
handle_call(start_agent, _From, State=#state{proxy=Proxy}) ->
    case State#state.agent of
        undefined ->
            case connman_agent:start_link(Proxy) of
                {ok, Agent} ->
                    {reply, ok, State#state{agent=Agent}};
                {error, Error} ->
                    {reply, {error, Error}, State}
            end;
        _ ->
            {reply, ok, State}
    end;

handle_call(Msg, _From, State=#state{}) ->
    lager:warning("Unhandled call ~p", [Msg]),
    {reply, ok, State}.



handle_cast(agent_cancel, State=#state{}) ->
    %% We do not handle the agent reporting a canceled connect since
    %% it mostly means that the corresponding connect call has timed
    %% out, ie. the caller has gone away.
    lager:info("Agent mentions a canceled connect"),
    {noreply, State};
handle_cast({agent_error, ServicePath, Error}, State=#state{}) ->
    case lists:keyfind(ServicePath, #connect.service_path, maps:values(State#state.connects)) of
        false ->
            lager:info("Ignoring agent error for unknown: ~p", [ServicePath]),
            {noreply, State};
        #connect{tech=Tech, pid=ConnectPid} ->
            {noreply, dispatch_connect_result(Tech, ConnectPid, Error, State)}
    end;
handle_cast({agent_retry, ServicePath}, State=#state{}) ->
    case lists:keyfind(ServicePath, #connect.service_path, maps:values(State#state.connects)) of
        false ->
            lager:info("Ignoring agent retry request for unknown: ~p", [ServicePath]),
            {noreply, State};
        #connect{tech=Tech, service_pass=ServicePass, handler=Handler} ->
            case start_connect(Tech,
                               {path, ServicePath}, ServicePass, Handler,
                               drop_connect(Tech, State)) of
                {ok, NewState} ->
                    {noreply, NewState};
                {Error, NewState} ->
                    lager:warning("Failed to start connect retry for ~p: ~p", [ServicePath, Error]),
                    {noreply, NewState}
            end
    end;

handle_cast(Msg, State=#state{}) ->
    lager:warning("Unhandled cast ~p", [Msg]),
    {noreply, State}.


handle_info({connect_service, Tech, ConnectPid, ServicePath}, State=#state{connects=Connects}) ->
    case maps:get(Tech, Connects, false) of
        false -> {noreply, State};
        C=#connect{pid=ConnectPid} ->
            NewConnects = maps:put(Tech, C#connect{service_path=ServicePath}, Connects),
            {noreply, State#state{connects=NewConnects}}
    end;
handle_info({connect_result, Tech, ConnectPid, Result}, State=#state{}) ->
    {noreply, dispatch_connect_result(Tech, ConnectPid, Result, State)};
handle_info({'DOWN', _Monitor, process, Pid, Error}, State=#state{}) when Error /= normal ->
    case lists:keyfind(Pid, #connect.pid, maps:values(State#state.connects)) of
        false ->
            {noreply, State};
        #connect{tech=Tech} ->
            {noreply, dispatch_connect_result(Tech, Pid, {error, Error}, State)}
    end;
handle_info(Msg, State=#state{}) ->
    lager:warning("Unhandled info ~p", [Msg]),
    {noreply, State}.

%%
%% Private
%%

-spec start_connect(technology(), {service_key(), string()}, string(), pid(), #state{})
                   -> {ok, #state{}} | {{error, term()}, #state{}}.
start_connect(Tech, Service, ServicePass, Handler, State=#state{connects=Connects}) ->
    case maps:get(Tech, Connects, false) of
        false ->
            {ok, ConnectPid} = connman_connect:start(State#state.proxy,
                                                     Tech, Service,
                                                     self()),
            ConnectMonitor = erlang:monitor(process, ConnectPid),
            NewConnects = maps:put(Tech,
                                   #connect{tech=Tech,
                                            pid=ConnectPid,
                                            monitor=ConnectMonitor,
                                            service_name=service_name(Service),
                                            service_path=service_path(Service),
                                            service_pass=ServicePass,
                                            handler=Handler},
                                   Connects),
            {ok, State#state{connects=NewConnects}};
        _ ->
            {{error, already_connecting}, State}
    end.

-spec drop_connect(technology(), #state{}) -> #state{}.
drop_connect(Tech, State=#state{}) ->
    case maps:take(Tech, State#state.connects) of
        error ->
            State;
        {#connect{monitor=Monitor}, Connects} ->
            erlang:demonitor(Monitor, [flush]),
            State#state{connects=Connects}
    end.

-spec dispatch_connect_result(technology(), pid(), term(), #state{}) -> #state{}.
dispatch_connect_result(Tech, ConnectPid, Result, State) ->
    case maps:take(Tech, State#state.connects) of
        {#connect{handler=Handler, pid=ConnectPid, monitor=Monitor}, Connects} ->
            erlang:demonitor(Monitor, [flush]),
            Handler ! {connect_result, Tech, Result},
            State#state{connects=Connects};
        _ ->
            State
    end.


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
