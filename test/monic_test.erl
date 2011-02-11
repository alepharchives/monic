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

-module(monic_test).
-include_lib("eunit/include/eunit.hrl").

basic_test_() ->
    {setup, fun setup/0, fun cleanup/1, fun write_read/1}.

setup() ->
    monic:start().

cleanup(_) ->
    monic:stop().

write_read(_) ->
    Group = "foo",
    Key = "bar",
    Bin = <<"hello this is a quick test">>,
    {ok, Handle} = monic:write(Group, Key, Bin),
    {ok, Bin1} = monic:read(Handle),
    ?assertEqual(Bin, Bin1).
