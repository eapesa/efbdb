-module(efbdb).
-behaviour(gen_server).

%% API exports
-export([
  connect/2,
  connect/3,
  add/1,
  add/2,
  add_no_key/1,
  add_no_key/2,
  remove/0,
  remove/1,
  update/2
]).
%% GenServer CALLBACKS
-export([
  start_link/3,
  init/1,
  terminate/2,
  code_change/3,
  handle_call/3,
  handle_cast/2,
  handle_info/2
]).
%% Shotgun Callback functions
-export([
  handle_events/3
]).

-type mf() :: {atom(), atom()}.

-record(state, {
  host            :: iodata(),
  node            :: iodata(),
  conn_sse        :: pid(),
  conn_http       :: pid(),
  secret          :: binary(),
  update_callback :: mf(),
  remove_callback :: mf()
}).

%%====================================================================
%% API functions
%%====================================================================
connect(Host, Opts) ->
  connect(Host, "", Opts).

connect(Host, Node, Opts) ->
  Config = [Host, Node, Opts],
  ChildSpec = {
      {local, ?MODULE},
      { ?MODULE, start_link, Config },
      permanent,
      brutal_kill,
      worker,
      [?MODULE]
    },
  supervisor:start_child(efbdb_sup, ChildSpec).

add(Data) ->
  add(Data, <<"/">>).

add(Data, Path) ->
  gen_server:call(?MODULE, {post, Path, Data}).

add_no_key(Data) ->
  add_no_key(Data, <<"/">>).

add_no_key(Data, Path) ->
  gen_server:call(?MODULE, {put, Path, Data}).

remove() ->
  remove(<<"/">>).

remove(Path) ->
  gen_server:call(?MODULE, {delete, Path, maps:new()}).

update(Data, Path) ->
  gen_server:call(?MODULE, {patch, Path, Data}).

%%====================================================================
%% Internal functions
%%====================================================================
handle_events(_FinNoFin, _Reference, Event) ->
  ParsedEvent = shotgun:parse_event(Event),
  gen_server:cast(?MODULE, {event,
      maps:get(event, ParsedEvent, <<>>),
      maps:get(data, ParsedEvent, <<>>)}).

start_link(Host, Node, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [{Host, Node, Opts}], []).

init([{Host, Node, Opts}]) ->
  Secret = utils:ensure_binary(maps:get(firebase_secret, Opts, <<>>)),
  UpdateCallback = maps:get(update_callback, Opts, {firebasedb, on_update}),
  RemoveCallback = maps:get(remove_callback, Opts, {firebasedb, on_remove}),
  {ok, #state{
    host            = Host,
    conn_sse        = false,
    conn_http       = false,
    node            = utils:ensure_binary(Node),
    secret          = utils:ensure_binary(Secret),
    update_callback = UpdateCallback,
    remove_callback = RemoveCallback
  }, 0}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

handle_call({Verb, Path, Data}, _From, #state{ conn_http=Conn, node=Node,
    secret=Secret }=State) ->
  Uri      = utils:ensure_list(manage_path(Node, Path, Secret)),
  Params   = generate_http_params(Verb, Conn, Uri, Data),
  Response = apply(shotgun, Verb, Params),
  {reply, Response, State};
handle_call(_Message, _From, State) ->
  {reply, ok, State}.

handle_cast({process_data, #{ <<"path">> := Path, <<"data">> := null }}, #state{
    remove_callback={Mod, Fun} }=State) ->
  spawn(Mod, Fun, [Path]),
  {noreply, State};
handle_cast({process_data, #{ <<"path">> := Path, <<"data">> := Data }}, #state{
    update_callback={Mod, Fun} }=State) ->
  spawn(Mod, Fun, [Path, Data]),
  {noreply, State};
handle_cast({event, <<"keep-alive">>, _Data}, State) ->
  %% NOTE: Keepalive todos
  {noreply, State};
handle_cast({event, Event, Data}, State) when (Event =:= <<"put">>);
                                              (Event =:= <<"patch">>) ->
  gen_server:cast(self(), {process_data, jsx:decode(Data, [return_maps])}),
  {noreply, State};
handle_cast({open_conns, {ok, SseConn}, {ok, HttpConn}},
    #state{ node=Node, secret=Secret }=State) ->
  Path     = manage_path(utils:ensure_binary(Node), Secret),
  _Result  = shotgun:get(SseConn, Path,
      #{<<"Accept">> => <<"text/event-stream">>,
        <<"Cache-Control">> => <<"no-cache">>},
      #{async => true, async_mode => sse,
        handle_event=> fun ?MODULE:handle_events/3}),
  {noreply, State#state{ conn_sse=SseConn, conn_http=HttpConn }};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(timeout, #state{ host=Host } = State) ->
  %% NOTE: Open two conns for shotgun. One is SSE listener, another is for HTTP.
  gen_server:cast(self(), {open_conns, shotgun:open(Host, 443, https),
      shotgun:open(Host, 443, https)}),
  {noreply, State};
handle_info(_Message, State) ->
  {noreply, State}.

% manage_path(Node) ->
%   manage_path(Node, <<"/">>, false).

manage_path(Node, Secret) ->
  manage_path(Node, <<"/">>, Secret).

manage_path(Node, Path, false) ->
  <<(join_path(Node, Path))/binary, ".json">>;
manage_path(Node, Path, Secret) ->
  <<(join_path(Node, Path))/binary, ".json?auth=", Secret/binary>>.

join_path(Node, <<"/">>) -> <<"/", Node/binary>>;
join_path(Node, Path) -> <<"/", Node/binary, "/", Path/binary>>.

get_headers(Verb) when (Verb =:= post); (Verb =:= put) ->
  #{ <<"Content-Type">> => <<"application/json">> };
get_headers(_Verb) -> #{}.

get_body(Verb, _Data) when (Verb =:= get); (Verb =:= delete) -> #{};
get_body(_Verb, Data) -> jsx:encode(Data).

generate_http_params(Verb, Conn, Uri, Data) when (Verb =:= get);
                                                 (Verb =:= delete) ->
  Headers = get_headers(Verb),
  Body    = get_body(Verb, Data),
  [Conn, Uri, Headers, Body];
generate_http_params(Verb, Conn, Uri, Data) ->
  Headers = get_headers(Verb),
  Body    = get_body(Verb, Data),
  [Conn, Uri, Headers, Body, #{}].
