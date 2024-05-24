-module(cosmerl_bech_tests).
-include_lib("eunit/include/eunit.hrl").

encode_bech32_valid_test_() ->
  [
    ?_assertEqual({ok, <<"a12uel5l">>},
      cosmerl_bech:encode(<<"a">>, <<>>)),

    ?_assertEqual({ok, <<"a12uel5l">>},
      cosmerl_bech:encode(<<"a">>, <<>>, [{char_case, lower}])),

    ?_assertEqual({ok, <<"A12UEL5L">>},
      cosmerl_bech:encode(<<"a">>, <<>>, [{char_case, upper}])),

    ?_assertEqual({ok, <<"?1ezyfcl">>},
      cosmerl_bech:encode(<<"?">>, <<>>)),

    ?_assertEqual({ok, <<"!1wctc0x">>},
      cosmerl_bech:encode(<<33>>, <<>>)),

    ?_assertEqual({ok, <<"~1qszm75">>},
      cosmerl_bech:encode(<<126>>, <<>>)),

    ?_assertEqual({ok, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa17vhfd0">>},
      cosmerl_bech:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<>>)),

    ?_assertEqual({ok, <<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs">>},
      cosmerl_bech:encode(<<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio">>, <<>>)),

    ?_assertEqual({ok, <<"abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw">>},
      cosmerl_bech:encode(<<"abcdef">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>)),

    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j">>},
      cosmerl_bech:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>)),

    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqzqx55gg7">>},
      cosmerl_bech:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16>>)),

    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqrudk4q8v">>},
      cosmerl_bech:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31>>)),

    ?_assertEqual({ok, <<"11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllluyng2g6">>},
      cosmerl_bech:encode(<<"1">>, <<255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255>>)),

    ?_assertEqual({ok, <<"split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w">>},
      cosmerl_bech:encode(<<"split">>, <<197, 243, 139, 112, 48, 95, 81, 155, 246, 109, 133, 251, 108, 240, 48, 88, 243, 221, 228, 99, 236, 215, 145, 143, 45, 199, 67, 145, 143, 45>>)),

    ?_assertEqual({ok, <<"ordered1qpzry9x8gf2tvdw0s3jn54khce6mua7lnzq8ey">>},
      cosmerl_bech:encode(<<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>)),

    ?_assertEqual({ok, <<"ORDERED1QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7LNZQ8EY">>},
      cosmerl_bech:encode(<<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>, [{char_case, upper}]))
  ].

encode_bech32_invalid_test_() ->
  [
    % no HRP
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:encode(<<"">>, <<>>)),

    % no HRP
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:encode(<<>>, <<>>)),

    % invalid character 0x20 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 32}},
      cosmerl_bech:encode(<<16#20>>, <<>>)),

    % invalid character 0x7F in HRP
    ?_assertEqual({error, {invalid_hrp_character, 127}},
      cosmerl_bech:encode(<<16#7F>>, <<>>)),

    % invalid character 0x80 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 128}},
      cosmerl_bech:encode(<<16#80>>, <<>>)),

    % HRP is too long, maximum allowed length is 83 characters
    ?_assertEqual({error, {hrp_length_exceeded, 84}},
      cosmerl_bech:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<>>)),

    % total length of the address is exceeded, maximum allowed length is 90 characters
    ?_assertEqual({error, {total_length_exceeded, 91}},
      cosmerl_bech:encode(<<"abcd">>, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>))
  ].

decode_bech32_valid_test_() ->
  [
    ?_assertEqual({ok, <<"a">>, <<>>},
      cosmerl_bech:decode(<<"a12uel5l">>)),

    ?_assertEqual({ok, <<"a">>, <<>>},
      cosmerl_bech:decode(<<"A12UEL5L">>)),

    ?_assertEqual({ok, <<"?">>, <<>>},
      cosmerl_bech:decode(<<"?1ezyfcl">>)),

    ?_assertEqual({ok, <<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio">>, <<>>},
      cosmerl_bech:decode(<<"an83characterlonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1tt5tgs">>)),

    ?_assertEqual({ok, <<"abcdef">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>},
      cosmerl_bech:decode(<<"abcdef1qpzry9x8gf2tvdw0s3jn54khce6mua7lmqqqxw">>)),

    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0>>},
      cosmerl_bech:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqc8247j">>)),

    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16>>},
      cosmerl_bech:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqzqx55gg7">>)),

    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31>>},
      cosmerl_bech:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqrudk4q8v">>)),

    ?_assertEqual({ok, <<"split">>, <<197, 243, 139, 112, 48, 95, 81, 155, 246, 109, 133, 251, 108, 240, 48, 88, 243, 221, 228, 99, 236, 215, 145, 143, 45, 199, 67, 145, 143, 45>>},
      cosmerl_bech:decode(<<"split1checkupstagehandshakeupstreamerranterredcaperred2y9e3w">>)),

    ?_assertEqual({ok, <<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>},
      cosmerl_bech:decode(<<"ordered1qpzry9x8gf2tvdw0s3jn54khce6mua7lnzq8ey">>)),

    ?_assertEqual({ok, <<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>},
      cosmerl_bech:decode(<<"ORDERED1QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7LNZQ8EY">>))
  ].

decode_bech32_invalid_test_() ->
  [
    % invalid character 0x20 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 32}},
      cosmerl_bech:decode(<<16#20, "1nwldj5">>)),

    % invalid character 0x7F in HRP
    ?_assertEqual({error, {invalid_hrp_character, 127}},
      cosmerl_bech:decode(<<16#7F, "1axkwrx">>)),

    % invalid character 0x80 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 128}},
      cosmerl_bech:decode(<<16#80, "1eym55h">>)),

    % total length of the address is exceeded, maximum allowed length is 90 characters
    ?_assertEqual({error, {total_length_exceeded, 91}},
      cosmerl_bech:decode(<<"an84characterslonghumanreadablepartthatcontainsthenumber1andtheexcludedcharactersbio1569pvx">>)),

    % no HRP separator (no letter '1')
    ?_assertEqual({error, no_hrp_separator},
      cosmerl_bech:decode(<<"pzry9x0s0muk">>)),

    % no HRP
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:decode(<<"1pzry9x0s0muk">>)),

    % invalid character in data (letter 'b')
    ?_assertEqual({error, {invalid_character, 98}},
      cosmerl_bech:decode(<<"x1b4n0q5v">>)),

    % too short checksum
    ?_assertEqual({error, too_short_checksum},
      cosmerl_bech:decode(<<"li1dgmt3">>)),

    % invalid character in checksum (letter 'b')
    ?_assertEqual({error, {invalid_character, 98}},
      cosmerl_bech:decode(<<"de1lg7wtb">>)),

    % checksum calculated with uppercase form of HRP, so is invalid
    ?_assertEqual({error, invalid_checksum},
      cosmerl_bech:decode(<<"A1G7SGD8">>)),

    % no HRP and no data
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:decode(<<"10a06t8">>)),

    % empty HRP with some data
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:decode(<<"1qzzfhee">>))
  ].

encode_bech32m_valid_test_() ->
  [
    ?_assertEqual({ok, <<"a1lqfn3a">>},
      cosmerl_bech:encode(<<"a">>, <<>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"a1lqfn3a">>},
      cosmerl_bech:encode(<<"a">>, <<>>, [{encoding, bech32m}, {char_case, lower}])),

    ?_assertEqual({ok, <<"A1LQFN3A">>},
      cosmerl_bech:encode(<<"a">>, <<>>, [{encoding, bech32m}, {char_case, upper}])),

    ?_assertEqual({ok, <<"?1v759aa">>},
      cosmerl_bech:encode(<<"?">>, <<>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"!1mym52y">>},
      cosmerl_bech:encode(<<33>>, <<>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"~14vjhmk">>},
      cosmerl_bech:encode(<<126>>, <<>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa1ts89gd">>},
      cosmerl_bech:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6">>},
      cosmerl_bech:encode(<<"an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber1">>, <<>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx">>},
      cosmerl_bech:encode(<<"abcdef">>, <<255, 187, 205, 235, 56, 189, 171, 73, 202, 48, 123, 154, 197, 169, 40, 57, 138, 65, 136, 32>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"11lllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllu30cxdc">>},
      cosmerl_bech:encode(<<"1">>, <<255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqzqngyydu">>},
      cosmerl_bech:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqruc29vzw">>},
      cosmerl_bech:encode(<<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"split1checkupstagehandshakeupstreamerranterredcaperredlc445v">>},
      cosmerl_bech:encode(<<"split">>, <<197, 243, 139, 112, 48, 95, 81, 155, 246, 109, 133, 251, 108, 240, 48, 88, 243, 221, 228, 99, 236, 215, 145, 143, 45, 199, 67, 145, 143, 45>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"ordered1qpzry9x8gf2tvdw0s3jn54khce6mua7lx7stux">>},
      cosmerl_bech:encode(<<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>, [{encoding, bech32m}])),

    ?_assertEqual({ok, <<"ORDERED1QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7LX7STUX">>},
      cosmerl_bech:encode(<<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>, [{encoding, bech32m}, {char_case, upper}]))
  ].

encode_bech32m_invalid_test_() ->
  [
    % no HRP
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:encode(<<"">>, <<>>, [{encoding, bech32m}])),

    % no HRP
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:encode(<<>>, <<>>, [{encoding, bech32m}])),

    % invalid character 0x20 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 32}},
      cosmerl_bech:encode(<<16#20>>, <<>>, [{encoding, bech32m}])),

    % invalid character 0x7F in HRP
    ?_assertEqual({error, {invalid_hrp_character, 127}},
      cosmerl_bech:encode(<<16#7F>>, <<>>, [{encoding, bech32m}])),

    % invalid character 0x80 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 128}},
      cosmerl_bech:encode(<<16#80>>, <<>>, [{encoding, bech32m}])),

    % HRP is too long, maximum allowed length is 83 characters
    ?_assertEqual({error, {hrp_length_exceeded, 84}},
      cosmerl_bech:encode(<<"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa">>, <<>>, [{encoding, bech32m}])),

    % total length of the address is exceeded, maximum allowed length is 90 characters
    ?_assertEqual({error, {total_length_exceeded, 91}},
      cosmerl_bech:encode(<<"abcd">>, <<"bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb">>, [{encoding, bech32m}]))
  ].

decode_bech32m_valid_test_() ->
  [
    ?_assertEqual({ok, <<"a">>, <<>>},
      cosmerl_bech:decode(<<"a1lqfn3a">>)),

    ?_assertEqual({ok, <<"a">>, <<>>},
      cosmerl_bech:decode(<<"A1LQFN3A">>)),

    ?_assertEqual({ok, <<"?">>, <<>>},
      cosmerl_bech:decode(<<"?1v759aa">>)),

    ?_assertEqual({ok, <<"!">>, <<>>},
      cosmerl_bech:decode(<<"!1mym52y">>)),

    ?_assertEqual({ok, <<"an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber1">>, <<>>},
      cosmerl_bech:decode(<<"an83characterlonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11sg7hg6">>)),

    ?_assertEqual({ok, <<"abcdef">>, <<255, 187, 205, 235, 56, 189, 171, 73, 202, 48, 123, 154, 197, 169, 40, 57, 138, 65, 136, 32>>},
      cosmerl_bech:decode(<<"abcdef1l7aum6echk45nj3s0wdvt2fg8x9yrzpqzd3ryx">>)),

    ?_assertEqual({ok, <<"1">>, <<255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255>>},
      cosmerl_bech:decode(<<"11llllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllludsr8">>)),

    ?_assertEqual({ok, <<"1">>, <<255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255>>},
      cosmerl_bech:decode(<<"11lllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllllu30cxdc">>)),

    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 16>>},
      cosmerl_bech:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqzqngyydu">>)),

    ?_assertEqual({ok, <<"1">>, <<0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 31>>},
      cosmerl_bech:decode(<<"11qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqrudk4q8v">>)),

    ?_assertEqual({ok, <<"split">>, <<197, 243, 139, 112, 48, 95, 81, 155, 246, 109, 133, 251, 108, 240, 48, 88, 243, 221, 228, 99, 236, 215, 145, 143, 45, 199, 67, 145, 143, 45>>},
      cosmerl_bech:decode(<<"split1checkupstagehandshakeupstreamerranterredcaperredlc445v">>)),

    ?_assertEqual({ok, <<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>},
      cosmerl_bech:decode(<<"ordered1qpzry9x8gf2tvdw0s3jn54khce6mua7lx7stux">>)),

    ?_assertEqual({ok, <<"ordered">>, <<0, 68, 50, 20, 199, 66, 84, 182, 53, 207, 132, 101, 58, 86, 215, 198, 117, 190, 119, 223>>},
      cosmerl_bech:decode(<<"ORDERED1QPZRY9X8GF2TVDW0S3JN54KHCE6MUA7LX7STUX">>))
  ].

decode_bech32m_invalid_test_() ->
  [
    % invalid character 0x20 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 32}},
      cosmerl_bech:decode(<<16#20, "1xj0phk">>)),

    % invalid character 0x7F in HRP
    ?_assertEqual({error, {invalid_hrp_character, 127}},
      cosmerl_bech:decode(<<16#7F, "1g6xzxy">>)),

    % invalid character 0x80 in HRP
    ?_assertEqual({error, {invalid_hrp_character, 128}},
      cosmerl_bech:decode(<<16#80, "1vctc34">>)),

    % total length of the address is exceeded, maximum allowed length is 90 characters
    ?_assertEqual({error, {total_length_exceeded, 91}},
      cosmerl_bech:decode(<<"an84characterslonghumanreadablepartthatcontainsthetheexcludedcharactersbioandnumber11d6pts4">>)),

    % no HRP separator (no letter '1')
    ?_assertEqual({error, no_hrp_separator},
      cosmerl_bech:decode(<<"qyrz8wqd2c9m">>)),

    % no HRP
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:decode(<<"1qyrz8wqd2c9m">>)),

    % invalid character in data (letter 'b')
    ?_assertEqual({error, {invalid_character, 98}},
      cosmerl_bech:decode(<<"y1b0jsk6g">>)),

    % invalid character in data (letter 'i')
    ?_assertEqual({error, {invalid_character, 105}},
      cosmerl_bech:decode(<<"lt1igcx5c0">>)),

    % too short checksum
    ?_assertEqual({error, too_short_checksum},
      cosmerl_bech:decode(<<"in1muywd">>)),

    % invalid character in checksum (letter 'i')
    ?_assertEqual({error, {invalid_character, 105}},
      cosmerl_bech:decode(<<"mm1crxm3i">>)),

    % invalid character in checksum (letter 'o')
    ?_assertEqual({error, {invalid_character, 111}},
      cosmerl_bech:decode(<<"au1s5cgom">>)),

    % checksum calculated with uppercase form of HRP, so is invalid
    ?_assertEqual({error, invalid_checksum},
      cosmerl_bech:decode(<<"M1VUXWEZ">>)),

    % no HRP and no data
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:decode(<<"16plkw9">>)),

    % empty HRP with some data
    ?_assertEqual({error, no_hrp},
      cosmerl_bech:decode(<<"1p2gdwpf">>))
  ].
