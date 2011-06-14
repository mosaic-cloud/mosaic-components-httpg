
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
% ----- gen_server wrappers
% ----------------------------------------------------------------------------


% ----- variables

-define (gen_server__init_wrapper__outcome_variable, GenServer_InitWrapper_OutcomeVariable).
-define (gen_server__terminate_wrapper__outcome_variable, GenServer_TerminateWrapper_OutcomeVariable).
-define (gen_server__code_change_wrapper__outcome_variable, GenServer_CodeChangeWrapper_OutcomeVariable).
-define (gen_server__handle_call_wrapper__outcome_variable, GenServer_HandleCallWrapper_OutcomeVariable).
-define (gen_server__handle_cast_wrapper__outcome_variable, GenServer_HandleCastWrapper_OutcomeVariable).
-define (gen_server__handle_info_wrapper__outcome_variable, GenServer_HandleInfoWrapper_OutcomeVariable).


% ----- init

-define (gen_server__init_wrapper (Argument, Body),
		init (Argument) ->
			begin
				_ = Argument,
				?gen_server__init_wrapper__outcome_variable = Body,
				?gen_server__init_wrapper__outcome_variable
			end).


% ----- handle_call

-define (gen_server__handle_call_wrapper (Message, Sender, State, Body),
		handle_call (Message, Sender, State) ->
			begin
				_ = Message, _ = Sender, _ = State,
				?gen_server__handle_call_wrapper__outcome_variable = Body,
				?gen_server__handle_call_wrapper__outcome_variable
			end).


% ----- handle_cast

-define (gen_server__handle_cast_wrapper (Message, State, Body),
		handle_cast (Message, State) ->
			begin
				_ = Message, _ = State,
				?gen_server__handle_cast_wrapper__outcome_variable = Body,
				?gen_server__handle_cast_wrapper__outcome_variable
			end).


% ----- handle_info

-define (gen_server__handle_info_wrapper (Message, State, Body),
		handle_info (Message, State) ->
			begin
				_ = Message, _ = State,
				?gen_server__handle_info_wrapper__outcome_variable = Body,
				?gen_server__handle_info_wrapper__outcome_variable
			end).


% ----- terminate

-define (gen_server__terminate_wrapper (Reason, State, Body),
		terminate (Reason, State) ->
			begin
				_ = Reason, _ = State,
				?gen_server__terminate_wrapper__outcome_variable = Body,
				?gen_server__terminate_wrapper__outcome_variable
			end).


% ----- code_change

-define (gen_server__code_change_wrapper (Version, State, Extra, Body),
		code_change (Version, State, Extra) ->
			begin
				_ = Version, _ = State, _ = Extra,
				?gen_server__code_change_wrapper__outcome_variable = Body,
				?gen_server__code_change_wrapper__outcome_variable
			end).


% ----------------------------------------------------------------------------
% ----- gen_server wrapper internal aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_internal_aliases).
	
	-define (init_wrapper (A1, A2), ?gen_server__init_wrapper (A1, A2)).
	-define (handle_call_wrapper (A1, A2, A3, A4), ?gen_server__handle_call_wrapper (A1, A2, A3, A4)).
	-define (handle_cast_wrapper (A1, A2, A3), ?gen_server__handle_cast_wrapper (A1, A2, A3)).
	-define (handle_info_wrapper (A1, A2, A3), ?gen_server__handle_info_wrapper (A1, A2, A3)).
	-define (terminate_wrapper (A1, A2, A3), ?gen_server__terminate_wrapper (A1, A2, A3)).
	-define (code_change_wrapper (A1, A2, A3, A4), ?gen_server__code_change_wrapper (A1, A2, A3, A4)).
	
-endif.


% ----- eof
