%%%----------------------------------------------------------------------------
%%%  Cryptographic functions
%%%----------------------------------------------------------------------------

-module(cosmerl_crypto).

-export([sha256/1]).

%%%----------------------------------------------------------------------------
%%%  Public functions
%%%----------------------------------------------------------------------------

% Returns SHA256 digest calculated on provided data.
%
-spec sha256(Data :: iodata()) -> Digest :: binary().
sha256(Data) ->
  crypto:hash(sha256, Data).

%%%----------------------------------------------------------------------------
%%% Unit tests
%%%----------------------------------------------------------------------------

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sha256_test_() ->
  [
    ?_assertEqual(<<188, 107, 253, 132, 142, 189, 120, 25, 201, 168, 43, 241, 36, 214, 94, 127, 115, 157, 8, 224, 2, 96, 30, 35, 187, 144, 106, 172, 212, 10, 61, 129>>, sha256("creator"))
  ].

-endif.
