-module(rt53_auth).
-behaviour(gen_server).

%% API
-export([start_link/2, start_link/0]).
-export([credentials/0, authinfo/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-include("../include/rt53.hrl").
-define(SERVER, ?MODULE).

-record(state, {aws_secret_access_key, aws_access_key_id}).

%% ------------------------- API.

stop() -> gen_server:cast(?MODULE, stop).
credentials() -> gen_server:call(?MODULE, credentials).
authinfo() -> gen_server:call(?MODULE, authinfo).


%% ------------------------- Callbacks.
start_link() ->
    start_link(os:getenv(?RT53_KEY_VAR), os:getenv(?RT53_SECRET_VAR)).

start_link(Key, Secret) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Key, Secret], []).

init([Key, Secret]) ->
    {ok, #state{aws_secret_access_key=Secret, aws_access_key_id=Key}}.

handle_call(credentials, _From, State) ->
    {reply, {State#state.aws_access_key_id,
             State#state.aws_secret_access_key}, State};
handle_call(authinfo, _From, State) ->
    HeaderValue = authorization_header(State#state.aws_access_key_id,
                                       State#state.aws_secret_access_key,
                                       aws_time()),
    {reply, HeaderValue, State}.

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ------------------------- Internal Functions.
authorization_header(Key, Secret, Date) ->
    <<Mac:160/integer>> = crypto:hmac(sha, Secret, Date),
    % crypto:sha_mac(Secret, Date)
    Payload = "AWS4-HMAC-SHA256
    Credential="++ Key ++ "/" ++ aws_date() ++ "/us-east-1/route53/aws4_request,
    SignedHeaders=host;range;x-amz-date,
    Signature=" ++ binary_to_list(base64:encode(list_to_binary(integer_to_list(Mac)))),
    {Payload, Date}.

aws_date() ->
    lists:flatten(io_lib:format("~w~2..0w~2..0w", tuple_to_list(date()))).

aws_time() ->
    {ok, {_Res, Headers, _Body}} =
        httpc:request(get, {rt53:aws_url("/date"), []}, [], []),
    proplists:get_value("date", Headers).
