%%%----------------------------------------------------------------------------
%%% Bank module (bank keeper)
%%%----------------------------------------------------------------------------

-module(cosmerl_bank).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------------
%%%  Public functions
%%%----------------------------------------------------------------------------

init(_Args) ->
  erlang:error(not_implemented).

handle_call(_Request, _From, _State) ->
  erlang:error(not_implemented).

handle_cast(_Request, _State) ->
  erlang:error(not_implemented).
