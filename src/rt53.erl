%%
%% rt53.erl
%% rt53 entry point
%%
-module(rt53).

-export([start/0, start_link/0, stop/0]).

start_link() ->
    rt53_sup:start_link().

start() ->
    application:start(rt53).

stop() ->
    application:stop(rt53).

