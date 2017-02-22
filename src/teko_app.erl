%%%-------------------------------------------------------------------
%%   _/_/_/_/_/  _/_/_/_/  _/    _/    _/_/
%%      _/      _/        _/  _/    _/    _/
%%     _/      _/_/_/    _/_/      _/    _/
%%    _/      _/        _/  _/    _/    _/
%%   _/      _/_/_/_/  _/    _/    _/_/
%%
%% @doc Main Teko application module
%%
%% @copyright 2017 Dennis Drown
%% @end
%%%-------------------------------------------------------------------
-module(teko_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("llog.hrl").

%%====================================================================
%% API
%%--------------------------------------------------------------------
-spec start(StartType :: term(),
            StartArgs :: term()) -> {ok, pid()}
                                  | {ok, pid(), term()}
                                  | {error, term()}.
%
% @doc  Main entry point for Teko
% @end  --
start(_StartType, _StartArgs) ->
    llog:start(),
    ?notice("Teko is opening up"),
    teko_sup:start_link().



%%--------------------------------------------------------------------
-spec stop(State :: term()) -> any().
%
% @doc Teko, close now!
% @end  --
stop(_State) ->
    ?notice("Teko is closed now"),
    ok.



%%====================================================================
%% Internal functions
%%====================================================================
