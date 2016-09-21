%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-record(var, {
	  name     :: term(),
	  type     :: atom(),
	  value    :: term(),
	  triggers :: [any()]
}).
