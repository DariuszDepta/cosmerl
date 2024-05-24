%%%----------------------------------------------------------------------------
%%% Top level supervisor of the blockchain
%%%----------------------------------------------------------------------------

-module(cosmerl_sup).

-behaviour(supervisor).

-export([start_link/0, init/1]).

-define(SERVER, ?MODULE).

%%%----------------------------------------------------------------------------
%%%  Public functions
%%%----------------------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    SupFlags = #{strategy => one_for_all, intensity => 0, period => 1},
    ChildSpecs = [],
    {ok, {SupFlags, ChildSpecs}}.
