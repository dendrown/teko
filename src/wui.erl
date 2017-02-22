%%-------------------------------------------------------------------
%%   _/_/_/_/_/  _/_/_/_/  _/    _/    _/_/
%%      _/      _/        _/  _/    _/    _/
%%     _/      _/_/_/    _/_/      _/    _/
%%    _/      _/        _/  _/    _/    _/
%%   _/      _/_/_/_/  _/    _/    _/_/
%%
%% @doc Web User Interface (WUI) server for Teko
%%
%% @copyright 2017 Dennis Drown
%% @end
%%%-------------------------------------------------------------------
-module(wui).
-behaviour(gen_server).

-export([start_link/0, stop/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("teko.hrl").
-include("llog.hrl").


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}
                    |  ignore
                    |  {error, term()}.
%%
% @doc  Startup function for the web interface module.
% @end  --
start_link() ->
    io:format("HERE<wui>~n"),
    gen_server:start_link({local, ?MODULE}, ?MODULE, [hello], []).


%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for the web interface
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).


%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init(atom()) -> {gen_init_rc(), term()}.
%%
% @doc  Initialization for the cortex server.
% @end  --
init(Args) ->
    ?notice("Web User Interface ON: args[~p]", Args),
    process_flag(trap_exit, true),
    {ok, []}.


%%--------------------------------------------------------------------
-spec terminate(Why   :: term(),
                State :: term()) -> ok.
%%
% @doc  Server shutdown callback.  Shuts down the AMQP queue and associated
%       channel and connection.
% @end  --
terminate(Why, _State) ->
    ?notice("Web User Interface OFF: why[~p]", [Why]),
    ok.


%%--------------------------------------------------------------------
-spec code_change(OldVsn :: term(),
                  State  :: term(),
                  Extra  :: term()) -> {atom(), term()}.
%%
% @doc  Hot code update processing: a placeholder.
% @end  --
code_change(OldVsn, State, _Extra) ->
    ?notice("Hot code update: old[~p]", [OldVsn]),
    {ok, State}.


%%--------------------------------------------------------------------
-spec handle_call(Msg   :: term(),
                  From  :: {pid(), term()},
                  State :: term()) -> {gen_call_rc(), term(), term()}
                                   |  {gen_call_rc(), term(), term(), gen_siesta()}.
%%
% @doc  Synchronous messages for the web user interface server.
% @end  --
handle_call(stop, _From, State) ->
    {stop, normal, ok, State};

handle_call(Msg, _From, State) ->
    ?warning("Unknown call: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
-spec handle_cast(Msg   :: term(),
                  State :: term()) -> {gen_cast_rc(), term()}
                                   |  {gen_cast_rc(), term(), gen_siesta()}.
%%
% @doc  Process async messages
% @end  --
handle_cast(Msg, State) ->
    ?warning("Unknown cast: ~p", [Msg]),
    {noreply, State}.


%%--------------------------------------------------------------------
-spec handle_info(Msg   :: term(),
                  State :: term()) -> {gen_info_rc(), term()}
                                   |  {gen_info_rc(), term(), gen_siesta()}.
%%
% @doc  Process out-of-band messages
% @end  --
handle_info(Msg, State) ->
    ?warning("Unknown info: ~p", [Msg]),
    {noreply, State}.


%%====================================================================
%% Internal functions
%%====================================================================
