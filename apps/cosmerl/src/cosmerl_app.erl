%%%-------------------------------------------------------------------
%%% @doc cosmerl public API
%%% @end
%%%-------------------------------------------------------------------

-module(cosmerl_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cosmerl_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
