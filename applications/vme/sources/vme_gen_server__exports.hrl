
%%%% Copyright © 2008, 2009, 2010, Ciprian Dorin Craciun <ciprian@volution.ro> <ciprian.craciun@gmail.com>
%%%%
%%%% This file is part of VEL (Volution Erlang Library).
%%%%
%%%% VEL is free software: you can redistribute it and / or modify
%%%% it under the terms of the GNU General Public License version 3
%%%% as published by the Free Software Foundation.
%%%%
%%%% VEL is distributed in the hope that it will be useful,
%%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%%% GNU General Public License version 3 for more details.
%%%%
%%%% You should have received a copy of the GNU General Public License
%%%% along with VEL.  If not, see <http://www.gnu.org/licenses/>.



% ----------------------------------------------------------------------------
% ----- gen_server exports
% ----------------------------------------------------------------------------


% ----- behaviour

-define (gen_server__behaviour,
		-behaviour (gen_server)).

-define (gen_server__export_behaviour,
		-export ([
			init/1, terminate/2, code_change/3,
			handle_call/3, handle_cast/2, handle_info/2])).


% ----- start

-define (gen_server__export_start_3,
		-export ([start/3])).

-define (gen_server__export_start_2,
		-export ([start/2])).

-define (gen_server__export_start_1,
		-export ([start/1])).

-define (gen_server__export_start_0,
		-export ([start/0])).


% ----- start_link

-define (gen_server__export_start_link_3,
		-export ([start_link/3])).

-define (gen_server__export_start_link_2,
		-export ([start_link/2])).

-define (gen_server__export_start_link_1,
		-export ([start_link/1])).

-define (gen_server__export_start_link_0,
		-export ([start_link/0])).


% ----- start_and_start_link

-define (gen_server__export_start_and_start_link_3,
		-export ([start/3, start_link/3])).

-define (gen_server__export_start_and_start_link_2,
		-export ([start/2, start_link/2])).

-define (gen_server__export_start_and_start_link_1,
		-export ([start/1, start_link/1])).

-define (gen_server__export_start_and_start_link_0,
		-export ([start/0, start_link/0])).


% ----- stop

-define (gen_server__export_stop_0,
		-export ([stop/0])).

-define (gen_server__export_stop_1,
		-export ([stop/1])).

-define (gen_server__export_stop_2,
		-export ([stop/2])).


% ----- call

-define (gen_server__export_call_s0 (Name),
		-export ([Name/1])).

-define (gen_server__export_call_s1 (Name),
		-export ([Name/2])).

-define (gen_server__export_call_s2 (Name),
		-export ([Name/3])).

-define (gen_server__export_call_s3 (Name),
		-export ([Name/4])).

-define (gen_server__export_call_s4 (Name),
		-export ([Name/5])).

-define (gen_server__export_call_s5 (Name),
		-export ([Name/6])).

-define (gen_server__export_call_0 (Name),
		-export ([Name/0])).

-define (gen_server__export_call_1 (Name),
		-export ([Name/1])).

-define (gen_server__export_call_2 (Name),
		-export ([Name/2])).

-define (gen_server__export_call_3 (Name),
		-export ([Name/3])).

-define (gen_server__export_call_4 (Name),
		-export ([Name/4])).

-define (gen_server__export_call_5 (Name),
		-export ([Name/5])).


% ---- cast

-define (gen_server__export_cast_s0 (Name),
		-export ([Name/1])).

-define (gen_server__export_cast_s1 (Name),
		-export ([Name/2])).

-define (gen_server__export_cast_s2 (Name),
		-export ([Name/3])).

-define (gen_server__export_cast_s3 (Name),
		-export ([Name/4])).

-define (gen_server__export_cast_s4 (Name),
		-export ([Name/5])).

-define (gen_server__export_cast_s5 (Name),
		-export ([Name/6])).

-define (gen_server__export_cast_0 (Name),
		-export ([Name/0])).

-define (gen_server__export_cast_1 (Name),
		-export ([Name/1])).

-define (gen_server__export_cast_2 (Name),
		-export ([Name/2])).

-define (gen_server__export_cast_3 (Name),
		-export ([Name/3])).

-define (gen_server__export_cast_4 (Name),
		-export ([Name/4])).

-define (gen_server__export_cast_5 (Name),
		-export ([Name/5])).


% ----------------------------------------------------------------------------
% ----- gen_server export internal aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_internal_aliases).
	
	-define (behaviour, ?gen_server__behaviour).
	-define (export_behaviour, ?gen_server__export_behaviour).
	
	-define (export_start_3, ?gen_server__export_start_3).
	-define (export_start_2, ?gen_server__export_start_2).
	-define (export_start_1, ?gen_server__export_start_1).
	-define (export_start_0, ?gen_server__export_start_0).
	
	-define (export_start_link_3, ?gen_server__export_start_link_3).
	-define (export_start_link_2, ?gen_server__export_start_link_2).
	-define (export_start_link_1, ?gen_server__export_start_link_1).
	-define (export_start_link_0, ?gen_server__export_start_link_0).
	
	-define (export_start_and_start_link_3, ?gen_server__export_start_and_start_link_3).
	-define (export_start_and_start_link_2, ?gen_server__export_start_and_start_link_2).
	-define (export_start_and_start_link_1, ?gen_server__export_start_and_start_link_1).
	-define (export_start_and_start_link_0, ?gen_server__export_start_and_start_link_0).
	
	-define (export_stop_0, ?gen_server__export_stop_0).
	-define (export_stop_1, ?gen_server__export_stop_1).
	-define (export_stop_2, ?gen_server__export_stop_2).
	
	-define (export_call_s0 (A1), ?gen_server__export_call_s0 (A1)).
	-define (export_call_s1 (A1), ?gen_server__export_call_s1 (A1)).
	-define (export_call_s2 (A1), ?gen_server__export_call_s2 (A1)).
	-define (export_call_s3 (A1), ?gen_server__export_call_s3 (A1)).
	-define (export_call_s4 (A1), ?gen_server__export_call_s4 (A1)).
	-define (export_call_s5 (A1), ?gen_server__export_call_s5 (A1)).
	
	-define (export_call_0 (A1), ?gen_server__export_call_0 (A1)).
	-define (export_call_1 (A1), ?gen_server__export_call_1 (A1)).
	-define (export_call_2 (A1), ?gen_server__export_call_2 (A1)).
	-define (export_call_3 (A1), ?gen_server__export_call_3 (A1)).
	-define (export_call_4 (A1), ?gen_server__export_call_4 (A1)).
	-define (export_call_5 (A1), ?gen_server__export_call_5 (A1)).
	
	-define (export_cast_s0 (A1), ?gen_server__export_cast_s0 (A1)).
	-define (export_cast_s1 (A1), ?gen_server__export_cast_s1 (A1)).
	-define (export_cast_s2 (A1), ?gen_server__export_cast_s2 (A1)).
	-define (export_cast_s3 (A1), ?gen_server__export_cast_s3 (A1)).
	-define (export_cast_s4 (A1), ?gen_server__export_cast_s4 (A1)).
	-define (export_cast_s5 (A1), ?gen_server__export_cast_s5 (A1)).
	
	-define (export_cast_0 (A1), ?gen_server__export_cast_0 (A1)).
	-define (export_cast_1 (A1), ?gen_server__export_cast_1 (A1)).
	-define (export_cast_2 (A1), ?gen_server__export_cast_2 (A1)).
	-define (export_cast_3 (A1), ?gen_server__export_cast_3 (A1)).
	-define (export_cast_4 (A1), ?gen_server__export_cast_4 (A1)).
	-define (export_cast_5 (A1), ?gen_server__export_cast_5 (A1)).
	
-endif.


% ----- eof
