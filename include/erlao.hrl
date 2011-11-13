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

%% @type ao_info() = #ao_info{}. Describes the attributes of an output
%% driver. Relevant members: <ul>
%%
%% <li><strong>type</strong> : the output type of the driver.</li>
%%
%% <li><strong>name</strong> : a longer name for the driver, which may
%% contain whitespaces, but no newlines. Useful for telling users what
%% driver is in use.</li>
%%
%% <li><strong>short_name</strong> : a short identifier for the
%% driver. Contains only alphanumeric characters, and no
%% whitespaces. It is used to lookup the driver ID number using {@link
%% driver_id/1}.</li>
%%
%% <li><strong>preferred_byte_format</strong> : specifies the
%% preffered ordering of sample bytes. Using the driver with this byte
%% format usually results in reduced memory and CPU usage. See {@link
%% ao_sample_format()} for a list of allowed values.</li>
%%
%% <li><strong>priority</strong> : a positive integer ranking how
%% likely it is for this driver to be the default one. The default
%% driver will be the functioning driver with the highest
%% priority.</li>
%%
%% <li><strong>comment</strong> : driver comment string. May contain
%% newlines.</li>
%%
%% <li><strong>options</strong> : a list of strings, which list the
%% option keys accepted by this driver.</li>
%%
%% <li><strong>option_count</strong> : length of the
%% <code>options</code> list.</li>
%%
%% </ul>
-record(ao_info, {
	  type :: ao_device_type(), % Live output or file output?
	  name :: string(), % Full name of driver
	  short_name :: string(), % Short name of driver
	  comment :: string(), % Driver comment
	  preferred_byte_format :: ao_endianness(),
	  priority :: integer(),
	  options :: [string()],
	  option_count :: integer()
}).

%% @type ao_sample_format() = #ao_sample_format{}. Describes the format of audio
%% samples. Relevant attributes : <ul>
%%
%% <li><strong>bits</strong> : number of bits per sample.</li>
%%
%% <li><strong>rate</strong> : samples per second per channel.</li>
%%
%% <li><strong>channels</strong> : number of audio channels.</li>
%%
%% <li><strong>byte_format</strong> : byte ordering in sample (see {@link ao_endianness()}).</li>
%%
%% <li><strong>matrix</strong> : input channel location and
%% ordering. See <a
%% href="http://xiph.org/ao/doc/ao_sample_format.html">the original
%% documentation</a> for complete details.</li>
%%
%% </ul>
-record(ao_sample_format, {
	  bits :: integer(), % Bits per sample
	  rate :: integer(), % Samples per second per channel
	  channels :: integer(), % Number of audio channels
	  byte_format :: ao_endianness(), % Byte ordering in sample
	  matrix :: string() % Input channel location/ordering
}).

-type(ao_error() :: enodriver | enotfile | enotlive | ebadoption |
		    eopendevice | eopenfile | efileexists |
		    ebadformat | efail).

-type(ao_endianness() :: big | little | native).

-type(ao_sample_format() :: #ao_sample_format{}).

-type(ao_device_type() :: file | live).

-type(ao_info() :: #ao_info{}).

-type(ao_option() :: {string(), string()}).

%% @type ao_device(). An opaque type that holds all the data for an
%% open device.
-opaque(ao_device() :: binary()).

-export_type([ao_error/0, ao_device_type/0, ao_endianness/0,
	      ao_info/0, ao_sample_format/0, ao_option/0, ao_device/0]).
