%%%-------------------------------------------------------------------
%%   _/_/_/_/_/  _/_/_/_/  _/    _/    _/_/
%%      _/      _/        _/  _/    _/    _/
%%     _/      _/_/_/    _/_/      _/    _/
%%    _/      _/        _/  _/    _/    _/
%%   _/      _/_/_/_/  _/    _/    _/_/
%%
%% @doc Top level Teko supervisor
%%
%% @copyright 2017 Dennis Drown
%% @end
%%%-------------------------------------------------------------------
-module(teko_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


%%====================================================================
%% API functions
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()}
                    | ignore
                    | {error, term()}.
%
% @doc  Sets up top level Teko supervisor
% @end  --
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


%%====================================================================
% Supervisor callbacks
%%--------------------------------------------------------------------
-spec init(list()) -> {ok, tuple()}.
%
% @doc  Sets up top level Teko supervisor
% @end  --
init([]) ->
    % Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
    {ok, { {all_for_one, 0, 1}, [{wui, {wui, start_link, []}, transient, 1000, worker, [wui]}
                                 | wui:get_child_specs()]}}.

%%====================================================================
%% Internal functions
%%====================================================================
