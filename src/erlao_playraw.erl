%% Copyright (C) 2011 Romain "Artefact2" Dalmaso <artefact2@gmail.com>
%% 
%% This program is free software: you can redistribute it and/or
%% modify it under the terms of the GNU General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful, but
%% WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%% General Public License for more details.
%%
%%  You should have received a copy of the GNU General Public License
%%  along with this program.  If not, see
%%  <http://www.gnu.org/licenses/>.

%% @private
-module(erlao_playraw).
-include("erlao.hrl").
-export([main/1]).

-define(BUFLEN, 32 * 1024).

main([Name]) ->
    io:format("Usage: ~s <bits> <rate> <channels> <byte_format>~n~n", [Name]),
    io:format("Example:~n~s 16 44100 2 little < test.raw~n~n", [Name]),
    io:format("Example (if you have xmp):~nxmp --output-file - any_module.xm | ~s 16 44100 2 little~n", [Name]);
main([_Name, SBits, SRate, SChannels, SByteFormat]) ->
    Bits = list_to_integer(SBits),
    Rate = list_to_integer(SRate),
    Channels = list_to_integer(SChannels),
    ByteFormat = list_to_atom(SByteFormat),
    application:start(erlao),
    ID = erlao:default_driver_id(),
    io:format("Available drivers: ~p~n", [extract_sname_list(erlao:driver_info_list())]),
    io:format("Using driver: ~s~n", [(erlao:driver_info(ID))#ao_info.name]),
    io:format("Using ~w bits per sample (endianness: ~w), ~w Hz rate and ~w channels.~nReading raw samples from stdin...~n", [Bits, ByteFormat, Rate, Channels]),
    Handle = erlao:open_live(ID, #ao_sample_format{bits = Bits, rate = Rate, channels = Channels, byte_format = ByteFormat, matrix = ""}, []),
    play_stdin(Handle),
    ok = erlao:close(Handle).

extract_sname_list([]) ->
    [];
extract_sname_list([Info | T]) ->
    [Info#ao_info.short_name | extract_sname_list(T)].

play_stdin(Handle) ->
    case file:read(standard_io, ?BUFLEN) of
	{ok, Data} ->
	    erlao:play(Handle, list_to_binary(Data)),
	    play_stdin(Handle);
	eof ->
	    ok
    end.
