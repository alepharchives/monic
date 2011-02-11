% Copyright 2011 Cloudant
%
% Licensed under the Apache License, Version 2.0 (the "License"); you may not
% use this file except in compliance with the License. You may obtain a copy of
% the License at
%
%   http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
% WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
% License for the specific language governing permissions and limitations under
% the License.

-module(monic).
-export([start/0, stop/0, read/1, write/3, delete/1]).

start() ->
    application:start(monic).

stop() ->
    application:stop(monic).

read(Handle) ->
    gen_server:call(monic_server, {read, Handle}, infinity).

write(Group, Key, Bin) when is_binary(Bin) ->
    gen_server:call(monic_server, {write, Group, Key, Bin}, infinity);
write(Group, Key, Fun) when is_function(Fun) ->
    gen_server:call(monic_server, {write, Group, Key, Fun}, infinity).

delete(Handle) ->
    gen_server:call(monic_server, {delete, Handle}, infinity).
