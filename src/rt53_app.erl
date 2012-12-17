%%
%% rt53_app.erl
%% rt53 application
%%
-module(rt53_app).
-include("../include/rt53.hrl").

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ~~~~~~~~~~~~~~~~~~~~~
%% Application callbacks
%% ~~~~~~~~~~~~~~~~~~~~~

start(_StartType, _StartArgs) ->
    {ok, Key} = application:get_env(?RT53_KEY_VAR),
    {ok, Secret} = application:get_env(?RT53_SECRET_VAR),
    rt53_sup:start_link(Key, Secret).

stop(_State) ->
    ok.

