%%%----------------------------------------------------------------------------
%%% Bank module
%%%
%%% This is the equivalent of the bank keeper in Cosmos-SDK.
%%%----------------------------------------------------------------------------

-module(cosmerl_bank).

-behaviour(gen_server).

% bank module public interface
-export([start_link/0]).

% gen_server behaviour callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------------
%%%  Public functions
%%%----------------------------------------------------------------------------

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  process_flag(trap_exit, true),
  io:format("DDD: starting '~p'~n", [?SERVER]),
  {ok, 0}.

handle_call(_Request, _From, State) ->
  {reply, [], State}.

handle_cast(_Request, State) ->
  {noreply, State}.

handle_info(_Request, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  io:format("DDD: stopping '~p'~n", [?SERVER]),
  ok.
