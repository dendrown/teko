%%%-------------------------------------------------------------------
%%   _/_/_/_/_/  _/_/_/_/  _/    _/    _/_/
%%      _/      _/        _/  _/    _/    _/
%%     _/      _/_/_/    _/_/      _/    _/
%%    _/      _/        _/  _/    _/    _/
%%   _/      _/_/_/_/  _/    _/    _/_/
%%
%% @doc Web User Inferface Definitions
%%
%% @copyright 2017 Dennis Drown
%% @end
%%%-------------------------------------------------------------------
-ifndef(_wui_included).
-define(_wui_included, ack).

-record(yaws_conf, {id          :: binary() | string(),
                    gConf       :: tuple(),
                    sConfs      :: [tuple()],
                    childSpecs  :: [tuple()]}).
-type yaws_conf() :: #yaws_conf{}.

-endif.
