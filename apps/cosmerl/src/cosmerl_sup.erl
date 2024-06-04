%%%----------------------------------------------------------------------------
%%% Top level supervisor of the blockchain
%%%----------------------------------------------------------------------------

-module(cosmerl_sup).

-behaviour(supervisor).

% supervisor public interface
-export([start_link/0]).

% supervisor behaviour callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------------
%%%  Public functions
%%%----------------------------------------------------------------------------

start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
  % supervisor
  SupFlags = #{
    strategy => one_for_one,
    intensity => 3,
    period => 10
  },
  % `account` worker
  CosmerlAccountSpec = #{
    id => account,
    start => {cosmerl_account, start_link, []},
    restart => permanent,
    shutdown => 10000,
    type => worker,
    modules => [cosmerl_account]},
  % `bank` worker
  CosmerlBankSpec = #{
    id => bank,
    start => {cosmerl_bank, start_link, []},
    restart => permanent,
    shutdown => 10000,
    type => worker,
    modules => [cosmerl_bank]},
  ChildSpecs = [CosmerlAccountSpec, CosmerlBankSpec],
  {ok, {SupFlags, ChildSpecs}}.
