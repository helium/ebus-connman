-module(connman_sup).

-behaviour(supervisor).

-export([start_link/0, start_link/1, init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    {ok, Bus} = ebus:system(),
    start_link(Bus).

-spec start_link(ebus:bus()) -> {ok, pid()}.
start_link(Bus) ->
    supervisor:start_link(?MODULE, [Bus]).

init([Bus]) ->
    ChildSpecs = [
                  #{ id => connman,
                     start => {connman, start_link, [Bus]},
                     restart => permanent,
                     shutdown => 1000,
                     type => worker
                   },
                  #{ id => services,
                     start => {connman_services, start_link, [Bus]},
                     restart => permanent,
                     type => worker
                   }
                 ],
    {ok, {{one_for_all, 1, 5}, ChildSpecs}}.
