%% Copyright (c) 2009
%% Bill Warnecke <bill@rupture.com>
%% Jacob Vorreuter <jacob.vorreuter@gmail.com>
%%
%% Copyright (c) 2013
%%
%% Permission is hereby granted, free of charge, to any person
%% obtaining a copy of this software and associated documentation
%% files (the "Software"), to deal in the Software without
%% restriction, including without limitation the rights to use,
%% copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following
%% conditions:
%%
%% The above copyright notice and this permission notice shall be
%% included in all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
%% EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
%% OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
%% NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
%% HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
%% WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
%% OTHER DEALINGS IN THE SOFTWARE.
%%==============================================================================
-module(emysql_util).

-export([encode/1, encode/2, quote/1]).

%% NOTE: these may not work with multi-byte encodings. utf8 is ok.

%%------------------------------------------------------------------------------
encode(Val) ->
  %% default return type to input type
  encode(Val, is_binary(Val)).

%% encode(Val, ReturnBinary)
encode(Val, true) when is_binary(Val) ->
  quote(Val);
encode(Val, true) ->
  list_to_binary(encode(Val, false));
encode(Val, false) when Val == undefined; Val == null ->
  "NULL";
encode(Val, false) when is_binary(Val) ->
  quote(binary_to_list(Val));
encode(Val, false) when is_atom(Val) ->
  quote(atom_to_list(Val));
encode(Val, false) when is_list(Val) ->
  quote(Val);
encode(Val, false) when is_integer(Val) ->
  integer_to_list(Val);
encode(Val, false) when is_float(Val) ->
  hd(io_lib:format("~w", [Val]));
encode({Time1, Time2, Time3}, false) ->
  lists:flatten(two_digits([Time1, Time2, Time3]));
encode({{Year, Month, Day}, {Hour, Minute, Second}}, false) ->
  lists:flatten(two_digits([Year, Month, Day, Hour, Minute, Second]));
encode({datetime, Val}, false) ->
  encode(Val, false);
encode({date, Val}, false) ->
  encode(Val, false);
encode({time, Val}, false) ->
  encode(Val, false);
encode({bits, Val}, false) when is_integer(Val) ->
  lists:flatten(io_lib:format("b'~.2b'",[Val]));
encode({bits, Val}, false) when is_binary(Val) ->
  "b" ++ quote(binary_to_list(Val));
encode({bits, Val}, false) when is_list(Val) ->
  "b" ++ quote(Val).

%% @private
two_digits(Nums) when is_list(Nums) ->
  [two_digits(Num) || Num <- Nums];
two_digits(Num) ->
  [Str] = io_lib:format("~b", [Num]),
  case length(Str) of
    1 -> [$0 | Str];
    _ -> Str
  end.

%%------------------------------------------------------------------------------
quote(Bin) when is_binary(Bin) ->
  %% This can be much faster for "larger" binaries > 1KB.  For smaller binaries
  %% or binaries with high rate (20%) of escape characters, it will be faster to
  %% use quote(binary_to_list(Bin)).  Benchmark your typical values and see.
  <<$', (binary:replace(Bin,
    [<<$'>>, <<$\\>>], <<$\\>>, [{insert_replaced, 1}, global]))/binary, $'>>;
quote(String) when is_list(String) ->
  [$' | do_quote(String) ++ [$']].
do_quote([$' | Rest]) ->
  [$\\, $' | do_quote(Rest)];
do_quote([$\\ | Rest]) ->
  [$\\, $\\ | do_quote(Rest)];
do_quote([C | Rest]) ->
  [C | do_quote(Rest)];
do_quote([]) ->
  [].
