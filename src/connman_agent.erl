-module(connman_agent).

-behavior(ebus_object).

-callback handle_input_request(State::any(), Path::string(), Specs::map()) -> Reply::map() | false.

-export([start_link/3, init/1]).
-export([handle_message/3, terminate/2]).

-record(state, {
                proxy :: ebus:proxy(),
                agent_name :: string(),
                input_module :: atom(),
                input_state :: any()
               }).

start_link(Proxy, InputModule, InputState) ->
    AgentName = "/net/helium/connmanctl" ++ integer_to_list(erlang:system_time(millisecond)),
    ebus_object:start_link(ebus_proxy:bus(Proxy), AgentName, ?MODULE,
                           [Proxy, AgentName, InputModule, InputState], []).

init([Proxy, AgentName, InputModule, InputState]) ->
    {ok, _} = ebus_proxy:call(Proxy, "/", "net.connman.Manager.RegisterAgent",
                              [object_path], [AgentName]),
    self() ! find_service,
    {ok, #state{proxy=Proxy, agent_name=AgentName,
                input_module=InputModule, input_state=InputState}}.


handle_message("RequestInput", Msg, State=#state{input_module=InputModule, input_state=InputState}) ->
    case ebus_message:args(Msg) of
        {ok, [ServicePath, Specs]} ->
            case InputModule:handle_input_request(InputState, ServicePath, Specs) of
                false ->
                    lager:info("Failed to handle input request for ~p", [ServicePath]),
                    {reply_error,
                     "net.connman.Agent.Error.Canceled",
                     "unable to provide requested input", State};
                ReplyMap ->
                    lager:info("Responded to input request for path ~p", [ServicePath]),
                    {reply, [{dict, string, variant}], [ReplyMap], State}
            end;
        {error, Error} ->
            lager:warning("Error in input request: ~p", [Error]),
            {noreply, State}
    end;
handle_message(Member, _Msg, State=#state{}) ->
    lager:warning("Unhandled message ~p", [Member]),
    {noreply, State}.


terminate(_, State=#state{}) ->
    ebus_proxy:call(State#state.proxy, "/", "net.connman.Manager.UnRegisterAgent",
                    [object_path], [State#state.agent_name]).
