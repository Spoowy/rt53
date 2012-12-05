%%
%% rt53_app.erl
%% rt53 application
%%
-module(rt53_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ~~~~~~~~~~~~~~~~~~~~~
%% Application callbacks
%% ~~~~~~~~~~~~~~~~~~~~~

start(_StartType, _StartArgs) ->
    
    rt53_sup:start_link().

stop(_State) ->
    ok.

