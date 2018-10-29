-module(connman_sup).

-behaviour(supervisor).

-export([start_link/0, init/1, connman/0]).

connman() ->
    Children = supervisor:which_children(?MODULE),
    {connman, Pid, _, _} = lists:keyfind(connman, 1, Children),
    {ok, Pid}.

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpec = #{ id => connman,
                   start => {connman, start_link, []},
                   restart => temporary,
                   shutdown => 1000,
                   type => worker
                 },
    {ok, {{one_for_one, 1, 5}, [ChildSpec]}}.
