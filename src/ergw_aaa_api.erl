%% Copyright 2017, Travelping GmbH <info@travelping.com>
%%
%% This program is free software: you can redistribute it and/or modify
%% it under the terms of the GNU Lesser General Public License as
%% published by the Free Software Foundation, either version 3 of the
%% License, or (at your option) any later version.
%%
%% This program is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with this program. If not, see <http://www.gnu.org/licenses/>.

-module(ergw_aaa_api).

-export([initialize_apis/1]).

-include("include/ergw_aaa_profile.hrl").

-callback call(State :: #{}) ->
    {ok, NewState :: #{}} |
    {stop, Reason :: term(), NewState :: #{}} |
    {next_profile, NextProfileName :: atom(), NewState :: #{}}.

initialize_apis(Config) ->
    Apis = proplists:get_value(apis, Config),
    lists:foldr(fun({API, Opts}, Specs) ->
			{ok, SupSpecs} = API:initialize_api(Opts),
			Specs ++ SupSpecs
		end, [], Apis).

%%===================================================================
%% Internal
%%===================================================================
