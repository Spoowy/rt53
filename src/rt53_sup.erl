%%
%% rt53_sup.erl
%% rt53 supervisor
%%
-module(rt53_sup).
-behaviour(supervisor).

%% API
-export([start_link/2]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILDARGS(I, Type, Key, Secret), 
        {I, {I, start_link, [Key, Secret]}, permanent, 5000, Type, [I]}).

%% ~~~~~~~~~~~~~
%% API functions
%% ~~~~~~~~~~~~~
start_link(Key, Secret) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, [Key, Secret]).

%% ~~~~~~~~~~~~~~~~~~~~ 
%% Supervisor callbacks
%% ~~~~~~~~~~~~~~~~~~~~

init([Key, Secret]) ->
    do_init(?CHILDARGS(rt53_auth, worker, Key, Secret)).

do_init(Server) ->
    crypto:start(),
    inets:start(),
    ssl:start(),
    {ok, { {one_for_one, 5, 10}, [Server]} }.

