-module(connman_agent).

-behavior(ebus_object).

-callback handle_input_request(Path::string(), Specs::map()) -> Reply::map() | false.

-export([start_link/1, init/1]).
-export([handle_message/3, terminate/2]).

-record(state, {
                proxy :: ebus:proxy(),
                agent_name :: string()
               }).

start_link(Proxy) ->
    AgentName = "/net/helium/connmanctl" ++ integer_to_list(erlang:system_time(millisecond)),
    ebus_object:start_link(ebus_proxy:bus(Proxy), AgentName, ?MODULE,
                           [Proxy, AgentName], []).

init([Proxy, AgentName]) ->
    {ok, _} = ebus_proxy:call(Proxy, "/", "net.connman.Manager.RegisterAgent",
                              [object_path], [AgentName]),
    self() ! find_service,
    {ok, #state{proxy=Proxy, agent_name=AgentName}}.


handle_message("net.connman.Agent.RequestInput", Msg, State=#state{}) ->
    case ebus_message:args(Msg) of
        {ok, [ServicePath, Specs]} ->
            case connman:handle_input_request(ServicePath, Specs) of
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
