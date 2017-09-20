-module(efbdb).
-behaviour(gen_server).

%% API exports
-export([
  connect/2,
  connect/3
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

-record(conn_state, {
  host :: iodata(),
  node :: iodata(),
  opts :: map()
}).
-record(state, {
  conn            :: pid(),
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
  Ret = supervisor:start_child(efbdb_sup, ChildSpec),
  io:format("[efbdb] Starting... ~p~n", [Ret]).

%%====================================================================
%% Internal functions
%%====================================================================
handle_events(_FinNoFin, _Reference, Event) ->
  ParsedEvent = shotgun:parse_event(Event),
  gen_server:cast(?MODULE, {event, maps:get(event, ParsedEvent, <<>>),
    maps:get(data, ParsedEvent, <<>>)}).

start_link(Host, Node, Opts) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [{Host, Node, Opts}], []).

init([{Host, Node, Opts}]) ->
  {ok, #conn_state{
    host = Host,
    node = Node,
    opts = Opts
  }, 0}.

terminate(_Reason, _State) -> ok.

code_change(_OldVsn, State, _Extra) -> {ok, State}.

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
handle_cast({open_conn, {ok, Conn}}, #conn_state{ node=Node, opts=Opts }=State) ->
  FbSecret = utils:ensure_binary(maps:get(firebase_secret, Opts, <<"x">>)),
  Path     = manage_path(utils:ensure_binary(Node), FbSecret),
  Result   = shotgun:get(Conn, Path,
      #{<<"Accept">> => <<"text/event-stream">>,
        <<"Cache-Control">> => <<"no-cache">>},
      #{async => true, async_mode => sse,
        handle_event=> fun ?MODULE:handle_events/3}),
  UpdateCallback = maps:get(update_callback, Opts, fun firebasedb:on_update/2),
  RemoveCallback = maps:get(remove_callback, Opts, fun firebasedb:on_remove/1),
  {noreply, #state{
    conn=Conn,
    update_callback=UpdateCallback,
    remove_callback=RemoveCallback
  }};
handle_cast(_Message, State) ->
  {noreply, State}.

handle_info(timeout, #conn_state{ host=Host } = State) ->
  gen_server:cast(self(), {open_conn, shotgun:open(Host, 443, https)}),
  {noreply, State};
handle_info(_Message, State) ->
  {noreply, State}.

manage_path(Node, <<"x">>) ->
  <<"/", Node/binary, ".json">>;
manage_path(Node, Secret) ->
  <<"/", Node/binary, ".json?auth=", Secret/binary>>.
