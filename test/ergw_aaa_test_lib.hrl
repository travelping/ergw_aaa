%% Copyright 2017, Travelping GmbH <info@travelping.com>

%% This program is free software; you can redistribute it and/or
%% modify it under the terms of the GNU General Public License
%% as published by the Free Software Foundation; either version
%% 2 of the License, or (at your option) any later version.

-define(equal(Expected, Actual),
    (fun (Expected@@@, Expected@@@) -> true;
	 (Expected@@@, Actual@@@) ->
	     ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p~nActual:   ~p~n",
		    [?FILE, ?LINE, ??Actual, Expected@@@, Actual@@@]),
	     false
     end)(Expected, Actual) orelse error(badmatch)).

-define(match(Guard, Expr),
	((fun () ->
		  case (Expr) of
		      Guard -> ok;
		      V -> ct:pal("MISMATCH(~s:~b, ~s)~nExpected: ~p ~nActual:   ~p~n",
				   [?FILE, ?LINE, ??Expr, ??Guard, V]),
			   error(badmatch)
		  end
	  end)())).

-ifndef(ERGW_AAA_NO_IMPORTS).
-import(ergw_aaa_test_lib, [meck_init/1, meck_reset/1, meck_unload/1, meck_validate/1,
			    set_cfg_value/3, get_cfg_value/2,
			    get_stats/1, diff_stats/2, wait_for_diameter/2,
			    outstanding_reqs/0]).
-endif.
