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

-export([start_link/1,
        stop/0,
        configure/0,
        get_conf/0]).
-export([init/1, terminate/2, code_change/3, handle_call/3, handle_cast/2, handle_info/2]).

-include("teko.hrl").
-include("llog.hrl").
-include("wui.hrl").


% TODO: Move config to the configuration
-define(ID,       "teko_wui").
-define(DOC_ROOT, "/tmp/teko/www").
-define(GCONFS,   [{id, ?ID},
                   {logdir, "/tmp/teko/log"}]).
-define(SCONFS,   [{port,       8080},
                   {servername, "teko"},
                   {listen,     {0,0,0,0}},
                   {docroot,    ?DOC_ROOT}]).

-record(state, {yaws :: yaws_conf()}).
-type state() :: #state{}.


%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start_link(yaws_conf()) -> {ok, pid()}
                              |  ignore
                              |  {error, term()}.
%%
% @doc  Startup function for the web interface module.
% @end  --
start_link(Conf) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Conf], []).


%%--------------------------------------------------------------------
-spec stop() -> ok.
%%
% @doc  Shutdown function for the web interface
% @end  --
stop() ->
    gen_server:call(?MODULE, stop).


%%--------------------------------------------------------------------
-spec configure() -> any().
%%
% @doc  Sets the configuration for the YAWS service.  Be sure the
%       supervisor has started the child processes before calling
%       this function.
% @end  --
configure() ->
    gen_server:call(?MODULE, configure).



%%--------------------------------------------------------------------
-spec get_conf() -> [tuple()].
%%
% @doc  Gets child process run specifications for YAWS processes.
%       A supervisor should call this function.
% @end  --
get_conf() ->
    {ok, SConfs, GConf, ChildSpecs} = yaws_api:embedded_start_conf(?DOC_ROOT,
                                                                   ?SCONFS,
                                                                   ?GCONFS,
                                                                   ?ID),
    Conf = #yaws_conf{id     = ?ID,
                      gConf  = GConf,
                      sConfs = SConfs,
                      childSpecs = ChildSpecs},

    ?info("YAWS: id[~p]", [Conf#yaws_conf.id]),

    %debug("YAWS: glob[~p]", [Conf#yaws_conf.gConf]),
    %debug("YAWS: srvs[~p]", [Conf#yaws_conf.sConfs]),
    %debug("YAWS: chSp[~p]", [Conf#yaws_conf.childSpecs]),
    Conf.



%%====================================================================
%% Server Implementation
%%--------------------------------------------------------------------
-spec init([yaws_conf()]) -> {gen_init_rc(), term()}.
%%
% @doc  Initialization for the cortex server.
% @end  --
init([Conf]) ->
    ?notice("Web User Interface ON: args[~p]", [Conf#yaws_conf.id]),
    process_flag(trap_exit, true),

    {ok, #state{yaws = Conf}}.


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
                  State :: state()) -> {gen_call_rc(), term(), term()}
                                    |  {gen_call_rc(), term(), term(), gen_siesta()}.
%%
% @doc  Synchronous messages for the web user interface server.
% @end  --
handle_call(configure, _From, State) ->
    Conf = State#state.yaws,
    ?info("Setting YAWS configuration ~s", [Conf#yaws_conf.id]),
    ConfStatus = yaws_api:setconf(Conf#yaws_conf.gConf, Conf#yaws_conf.sConfs),
    {reply, ConfStatus, State};


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
