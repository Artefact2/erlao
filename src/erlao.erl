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

%% @author Romain Dalmaso <artefact2@gmail.com>
%%
%% @doc This is an Erlang binding of the libao library.
%% See also http://xiph.org/ao/ and http://xiph.org/ao/doc/.
-module(erlao).
-behaviour(application).

%% @headerfile "erlao.hrl"
-include("erlao.hrl").

-export([append_global_option/1, open_live/3, open_file/5, play/2,
	 close/1, driver_id/1, default_driver_id/0, driver_info/1,
	 driver_info_list/0, file_extension/1, is_big_endian/0]).
-export([start/2, init/0, stop/1]).

%% @doc Append a key-value pair to the internal list of global options
%% obeyed by libao itself.
-spec(append_global_option(ao_option()) -> ok | efail).
append_global_option({Key, Value}) ->
    call_port({append_global_option, Key, Value}).

%% @doc Open a live playback audio device for output.
-spec(open_live(integer(), ao_sample_format(), [ao_option()]) -> ao_device() | ao_error()).
open_live(DriverId, SampleFormat, Options) ->
    call_port({open_live, DriverId, SampleFormat, Options}).

%% @doc Open a file for audio output. The file format is determined by
%% the audio driver used.
-spec(open_file(integer(), string(), boolean(), ao_sample_format(), [ao_option()]) -> ao_device() | ao_error()).
open_file(DriverId, Filename, Overwrite, SampleFormat, Options) ->
    call_port({open_file, DriverId, Filename, Overwrite, SampleFormat, Options}).

%% @doc Play a block of audio data to an open device. Samples are
%% interleaved by channels (Time 1, Channel 1; Time 1, Channel 2; Time
%% 2, Channel 1; etc.) in the memory buffer.
-spec(play(ao_device(), binary()) -> ok | efail).
play(Device, OutputSamples) ->
    call_port({play, Device, OutputSamples}).

%% @doc Closes the audio device and frees the memory allocated by the
%% device structure.
-spec(close(ao_device()) -> ok | efail).
close(Device) ->
    call_port({close, Device}).

%% @doc Looks up the ID number for a driver based upon its short
%% name. The ID number is need to open the driver or get info on it.
-spec(driver_id(string()) -> integer() | efail).
driver_id(ShortName) ->
    call_port({driver_id, ShortName}).

%% @doc Returns the ID number of the default live output driver. If
%% the <a href="http://xiph.org/ao/doc/config.html">configuration
%% files</a> specify a default driver, its ID is returned, otherwise
%% the library tries to pick a live output driver that will work on
%% the host platform.
-spec(default_driver_id() -> integer() | efail).
default_driver_id() ->
    call_port({default_driver_id}).

%% @doc Get information about a particular driver.
-spec(driver_info(integer()) -> ao_info() | efail).
driver_info(DriverId) ->
    call_port({driver_info, DriverId}).

%% @doc Get a list of {@link ao_info()} records for every available
%% driver.
-spec(driver_info_list() -> [ao_info()]).
driver_info_list() ->
    call_port({driver_info_list}).

%% @doc Returns the normal file extension associated with a particular
%% driver (like "wav" or "au"). This is just an information function
%% to allow library users to guess appropriate file names. You can
%% safely ignore the recommended extension.
%% 
%% <strong>Warning, this is broken at the moment and will always
%% return an error.</strong>
-spec(file_extension(integer()) -> string() | efail).
file_extension(DriverId) ->
    call_port({file_extension, DriverId}).

%% @doc Test if this computer uses big-endian byte ordering.
 -spec(is_big_endian() -> boolean()).
is_big_endian() ->
    call_port({is_big_endian}).

%% =====================================================

%% @doc Initialize the port and the libao library. The system and user
%% configuration files are read at this time. You must call {@link
%% start/0} before using any other function from this module. Do not
%% call it more than once though.
-spec(start(normal, any()) -> ok).
start(normal, _) ->
    Pid = spawn(erlao, init, []),
    register(erlao, Pid),
    {ok, Pid}.

%% @doc Unloads the libao library and closes the port. You must close
%% all previously opened devices before calling {@link stop/0}.
-spec(stop(any()) -> ok).
stop(_) ->
    erlao ! {stop, self()},
    receive ok -> ok end,
    ok.

%% =====================================================

-define(AO_TYPE_LIVE, 1).
-define(AO_TYPE_FILE, 2).

-define(AO_ENODRIVER, 1).
-define(AO_ENOTFILE, 2).
-define(AO_ENOTLIVE, 3).
-define(AO_EBADOPTION, 4).
-define(AO_EOPENDEVICE, 5).
-define(AO_EOPENFILE, 6).
-define(AO_EFILEEXISTS, 7).
-define(AO_EBADFORMAT, 8).
-define(AO_EFAIL, 100).

-define(AO_FMT_LITTLE, 1).
-define(AO_FMT_BIG, 2).
-define(AO_FMT_NATIVE, 4).

call_port(Call) ->
    erlao ! {call, self(), Call},
    receive
	{reply, Reply} ->
	    Reply
    end.

%% @private
init() ->
    Port = open_port({spawn_executable, "../priv/erlao_port"}, [hide, binary, nouse_stdio, {packet, 4}]),
    loop(Port).

loop(Port) ->
    receive
	{call, From, Call} ->
	    Port ! {self(), {command, encode(Call)}},
	    receive
		{Port, {data, Data}} ->
		    From ! {reply, decode(Data)}
	    end,
	    loop(Port);
	{stop, From} ->
	    Port ! {self(), close},
	    receive
		{Port, closed} ->
		    From ! ok,
		    exit(normal)
	    end;
	{'EXIT', Port, _Reason} ->
	    exit(port_terminated)
    end.

encode({is_big_endian}) ->
    <<1:8/unsigned-integer>>;
encode({append_global_option, Key, Value}) ->
    BKey = list_to_binary(Key),
    BVal = list_to_binary(Value),
    ValOffset = size(BKey) + 6,
    <<2:8/unsigned-integer, ValOffset:32/unsigned-integer-big, 
      BKey/binary, 0:8/signed-integer, 
      BVal/binary, 0:8/signed-integer>>;
encode({open_live, DriverId, SampleFormat, Options}) ->
    BSFormat = encode_sample_format(SampleFormat),
    BOptions = encode_option_list(Options),
    <<3:8/unsigned-integer, DriverId:32/signed-integer-big,
      (size(BSFormat) + 9):32/unsigned-integer-big,
      BSFormat/binary,
      BOptions/binary>>;
encode({open_file, DriverId, Filename, Overwrite, SampleFormat, Options}) ->
    BSFormat = encode_sample_format(SampleFormat),
    BOptions = encode_option_list(Options),
    IOverwrite = case Overwrite of
		     true -> 1;
		     false -> 0
		 end,
    BFilename = list_to_binary(Filename),
    SBF = size(BFilename) + 1,
    <<4:8/unsigned-integer, DriverId:32/signed-integer-big,
      IOverwrite:8/unsigned-integer,
      (SBF + 14):32/unsigned-integer-big,
      (SBF + size(BSFormat) + 14):32/unsigned-integer-big,
      BFilename/binary, 0:8/signed-integer,
      BSFormat/binary,
      BOptions/binary>>;
encode({play, Device, Samples}) ->
    <<5:8/unsigned-integer, (size(Samples)):32/unsigned-integer-big, Device/binary, Samples/binary>>;
encode({close, Device}) ->
    <<6:8/unsigned-integer, Device/binary>>;
encode({driver_id, ShortName}) ->
    BSN = list_to_binary(ShortName),
    <<7:8/unsigned-integer, BSN/binary, 0:8/signed-integer>>;
encode({default_driver_id}) ->
    <<8:8/unsigned-integer>>;
encode({driver_info, DriverId}) ->
    <<9:8/unsigned-integer, DriverId:32/signed-integer-big>>;
encode({driver_info_list}) ->
    <<10:8/unsigned-integer>>;
encode({file_extension, DriverId}) ->
    <<11:8/unsigned-integer, DriverId:32/signed-integer-big>>.

encode_option_list(List) ->
    encode_option_list(List, <<>>, <<>>, 0, 0).
encode_option_list([], BAcc, Offsets,  Count, _) ->
    <<Count:32/unsigned-integer-big,
      Offsets/binary,
      BAcc/binary>>;
encode_option_list([{K, V} | T], BAcc, Offsets, Count, GlobalOffset) ->
    BK = list_to_binary(K),
    BV = list_to_binary(V),
    SK = size(BK),
    SV = size(BV),
    encode_option_list(T, <<BAcc/binary, 
			    BK/binary, 0:8/signed-integer,
			    BV/binary, 0:8/signed-integer>>,
		       <<Offsets/binary, GlobalOffset:32/unsigned-integer-big,
			 (GlobalOffset + SK + 1):32/unsigned-integer-big>>,
		       Count + 1,
		       GlobalOffset + SK + SV + 2).

encode_sample_format(SampleFormat) ->
    Endianness = case SampleFormat#ao_sample_format.byte_format of
		     big ->
			 ?AO_FMT_BIG;
		     little ->
			 ?AO_FMT_LITTLE;
		     native ->
			 ?AO_FMT_NATIVE 
		 end,
    Matrix = list_to_binary(SampleFormat#ao_sample_format.matrix),
    <<(SampleFormat#ao_sample_format.bits):32/signed-integer-big,
      (SampleFormat#ao_sample_format.rate):32/signed-integer-big,
      (SampleFormat#ao_sample_format.channels):32/signed-integer-big,
      Endianness:32/signed-integer-big,
      Matrix/binary, 0:8/signed-integer>>.

decode(<<11:8/unsigned-integer, 1:8/unsigned-integer, Ext/binary>>) ->
    binary_to_list(Ext);
decode(<<11:8/unsigned-integer, 0:8/unsigned-integer>>) ->
    efail;
decode(<<10:8/unsigned-integer, BInfos/binary>>) ->
    decode_ao_infos(BInfos);
decode(<<9:8/unsigned-integer, 0:8/unsigned-integer>>) ->
    efail;
decode(<<9:8/unsigned-integer, 1:8/unsigned-integer,
	 BInfo/binary>>) ->
    [Info] = decode_ao_infos(BInfo),
    Info;
decode(<<8:8/unsigned-integer, Result:32/signed-integer-big>>) ->
    case Result of
	-1 ->
	    efail;
	Id ->
	    Id
    end;
decode(<<7:8/unsigned-integer, Result:32/signed-integer-big>>) ->
    case Result of
	-1 ->
	    efail;
	Id -> 
	    Id
    end;
decode(<<6:8/unsigned-integer, Result:8/unsigned-integer>>) ->
    case Result of
	1 ->
	    ok;
	0 ->
	    efail
    end;
decode(<<5:8/unsigned-integer, Result:8/unsigned-integer>>) ->
    case Result of
	0 -> 
	    efail;
	_ -> 
	    ok
    end;
decode(<<4:8/unsigned-integer, 0:8/unsigned-integer, Errno:32/signed-integer-big>>) ->
    int_to_ao_error(Errno);
decode(<<4:8/unsigned-integer, 1:8/unsigned-integer, Handle/binary>>) ->
    Handle;
decode(<<3:8/unsigned-integer, 0:8/unsigned-integer, Errno:32/signed-integer-big>>) ->
    int_to_ao_error(Errno);
decode(<<3:8/unsigned-integer, 1:8/unsigned-integer, Handle/binary>>) ->
    Handle;
decode(<<2:8/unsigned-integer, RetValue:8/unsigned-integer>>) ->
    case RetValue of
	1 ->
	    ok;
	0 ->
	    efail
    end;
decode(<<1:8/unsigned-integer, BigEndian:8/unsigned-integer>>) ->
    BigEndian == 1;
decode(<<0:8/unsigned-integer>>) ->
    unknown_method.

decode_ao_infos(Binary) ->
    decode_ao_infos(Binary, []).

decode_ao_infos(<<>>, Acc) ->
    Acc;
decode_ao_infos(<<Type:32/signed-integer-big,
		  LName:32/signed-integer-big,
		  BName:LName/binary,
		  LShortName:32/signed-integer-big,
		  BShortName:LShortName/binary,
		  LComment:32/signed-integer-big,
		  BComment:LComment/binary,
		  IPreferredByteFormat:32/signed-integer-big,
		  Priority:32/signed-integer-big,
		  OptionCount:32/signed-integer-big,
		  BOptions/binary>>,
		Acc) ->
    {Options, Rest} = decode_option_list(BOptions, OptionCount),
    decode_ao_infos(Rest, [#ao_info{
	type = decode_device_type(Type),
	name = binary_to_list(BName),
	short_name = binary_to_list(BShortName),
	comment = binary_to_list(BComment),
	preferred_byte_format = decode_byte_format(IPreferredByteFormat),
	priority = Priority,
	options = Options,
	option_count = OptionCount
       } | Acc]).

decode_option_list(Options, OptionCount) ->
    decode_option_list(Options, OptionCount, [], <<>>).

decode_option_list(Rest, 0, Options, <<>>) ->
    {Options, Rest};
decode_option_list(<<0:8/signed-integer, Rest/binary>>, OptionCount, Options, SoFar) ->
    decode_option_list(Rest, OptionCount - 1, [binary_to_list(SoFar) | Options], <<>>);
decode_option_list(<<X:8/signed-integer, Rest/binary>>, OptionCount, Options, SoFar) ->
    decode_option_list(Rest, OptionCount, Options, <<SoFar/binary, X:8/signed-integer>>).

decode_device_type(?AO_TYPE_FILE) ->
    file;
decode_device_type(?AO_TYPE_LIVE) ->
    live.

decode_byte_format(?AO_FMT_LITTLE) ->
    little;
decode_byte_format(?AO_FMT_BIG) ->
    big;
decode_byte_format(?AO_FMT_NATIVE) ->
    native.

int_to_ao_error(?AO_ENODRIVER) ->
    enodriver;
int_to_ao_error(?AO_ENOTFILE) ->
    enotfile;
int_to_ao_error(?AO_ENOTLIVE) ->
    enotlive;
int_to_ao_error(?AO_EBADOPTION) ->
    ebadoption;
int_to_ao_error(?AO_EOPENDEVICE) ->
    eopendevice;
int_to_ao_error(?AO_EOPENFILE) ->
    eopenfile;
int_to_ao_error(?AO_EFILEEXISTS) ->
    efileexists;
int_to_ao_error(?AO_EBADFORMAT) ->
    ebadformat;
int_to_ao_error(_) ->
    efail.








