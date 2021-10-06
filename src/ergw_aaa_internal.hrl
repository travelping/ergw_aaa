%% Copyright 2016, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-compile({parse_transform, cut}).

%% ErrLevel
-define(WARNING, 1).
-define(FATAL, 2).

-record(aaa_err, {
		  level,
		  where
		 }).

-define(AAA_ERR(Level), #aaa_err{level=Level,where={?FILE, ?LINE}}).

-record(peer_stats, {
    no_tokens   = 0,
    no_capacity = 0
}).

-record(peer, {
    outstanding = 0,
    capacity    = 50,
    last_ts     = undefined,
    rate        = 10,          %% requests per second
    interval    = 0,           %% refill interval in microseconds
    tokens      = 0,
    stats       = #peer_stats{}
}).
