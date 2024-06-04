%%%----------------------------------------------------------------------------
%%% Blockchain application
%%%----------------------------------------------------------------------------

-module(cosmerl_app).

-behaviour(application).

% application behaviour callbacks
-export([start/2, stop/1]).

%%%----------------------------------------------------------------------------
%%%  Public functions
%%%----------------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    cosmerl_sup:start_link().

stop(_State) ->
    ok.
