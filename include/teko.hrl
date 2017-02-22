%%%-------------------------------------------------------------------
%%   _/_/_/_/_/  _/_/_/_/  _/    _/    _/_/
%%      _/      _/        _/  _/    _/    _/
%%     _/      _/_/_/    _/_/      _/    _/
%%    _/      _/        _/  _/    _/    _/
%%   _/      _/_/_/_/  _/    _/    _/_/
%%
%% @doc Common settingr for Teko
%%
%% @copyright 2017 Dennis Drown
%% @end
%%%-------------------------------------------------------------------
-ifndef(_teko_included).
-define(_teko_included, ack).

% gen_server types
-type gen_init_rc() :: ok | stop.
-type gen_call_rc() :: reply | noreply | stop.
-type gen_cast_rc() :: noreply | stop.
-type gen_info_rc() :: noreply | stop.

-type gen_siesta()  :: hibernate | infinity | integer().

-endif.
