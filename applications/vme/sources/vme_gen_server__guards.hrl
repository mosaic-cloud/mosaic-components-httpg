
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
% ----- gen_server guards
% ----------------------------------------------------------------------------


% ----- variables

-define (gen_server__init_guard__errors_variable, GenServer_InitGuard_ErrorsVariable).
-define (gen_server__handle_call_guard__errors_variable, GenServer_HandleCallGuard_ErrorsVariable).
-define (gen_server__handle_cast_guard__errors_variable, GenServer_HandleCastGuard_ErrorsVariable).
-define (gen_server__handle_info_guard__errors_variable, GenServer_HandleInfoGuard_ErrorsVariable).


% ----- init

-define (gen_server__init_guard (Argument, Body, Validation),
		case Validation of
			ok ->
				Body;
			{error, ?gen_server__init_guard__errors_variable} ->
				{stop, {error, {invalid_argument, ?gen_server__init_guard__errors_variable}}}
		end).


% ----- handle_call

-define (gen_server__handle_call_guard (Request, Sender, State, Body, Validation),
		case Validation of
			ok ->
				Body;
			{error, ?gen_server__handle_call_guard__errors_variable} ->
				{reply, {error, {invalid_request, ?gen_server__handle_call_guard__errors_variable}}, State}
		end).


% ----- handle_call

-define (gen_server__handle_cast_guard (Request, State, Body, Validation),
		case Validation of
			ok ->
				Body;
			{error, _} ->
				{noreply, State}
		end).


% ----- handle_info

-define (gen_server__handle_info_guard (Message, State, Body, Validation),
		case Validation of
			ok ->
				Body;
			{error, _} ->
				{noreply, State}
		end).


% ----------------------------------------------------------------------------
% ----- gen_server guard internal aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_internal_aliases).
	
	-define (init_guard, ?gen_server__init_guard).
	-define (handle_call_guard, ?gen_server__handle_call_guard).
	-define (handle_cast_guard, ?gen_server__handle_cast_guard).
	-define (handle_info_guard, ?gen_server__handle_info_guard).
	
-endif.


% ----- eof
