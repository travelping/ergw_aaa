%% Copyright 2018, Travelping GmbH <info@travelping.com>
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

-record(aaa_request, {
		      from		:: {pid(), reference()},
		      handler		:: atom(),
		      procedure,
		      session,
		      events = []
		     }).

-define(get_svc_opt(Key, Session, Default),
	ergw_aaa_session:get_svc_opt(?MODULE, Key, Session, Default)).
-define(set_svc_opt(Key, Value, Session),
	ergw_aaa_session:set_svc_opt(?MODULE, Key, Value, Session)).

-define(get_svc_all_opt(Session),
	ergw_aaa_session:get_svc_opt(?MODULE, Session)).
-define(set_svc_all_opt(Opts, Session),
	ergw_aaa_session:set_svc_opt(?MODULE, Opts, Session)).
