
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
% ----- gen_server module
% ----------------------------------------------------------------------------

-ifdef (gen_server__module).
	
	
	% ----- server defaults
	
	-ifdef (server_default_name).
		
		-ifdef (server_default_local).
			-ifdef (server_default_global).
				-error (server_default_both_local_and_global).
			-else.
				-define (server_default_type, local).
				-define (server_default_start_name, {local, ?server_default_name}).
				-define (server_default_send_name, ?server_default_name).
			-endif.
		-else.
			-ifdef (server_default_global).
				-define (server_default_type, global).
				-define (server_default_start_name, {global, ?server_default_name}).
				-define (server_default_send_name, {global, ?server_default_name}).
			-else.
				-define (server_default_type, local).
				-define (server_default_start_name, {local, ?server_default_name}).
				-define (server_default_send_name, ?server_default_name).
			-endif.
		-endif.
		
	-endif.
	
	
	-ifdef (server_default_name).
		-define (gen_server__server_default_name, ?server_default_name).
	-endif.
	
	-ifdef (server_default_start_name).
		-define (gen_server__server_default_start_name, ?server_default_start_name).
	-endif.
	
	-ifdef (server_default_send_name).
		-define (gen_server__server_default_send_name, ?server_default_send_name).
	-endif.
	
	
	-ifdef (server_default_start_argument).
		-define (gen_server__server_default_start_argument, ?server_default_start_argument).
	-endif.
	
	-ifdef (server_default_start_options).
		-define (gen_server__server_default_start_options, ?server_default_start_options).
	-endif.
	
	
	-ifdef (server_default_stop_reason).
		-define (gen_server__server_default_stop_reason, ?server_default_stop_reason).
	-endif.
	
	
	% ----- server specs
	
	-ifndef (server_start_argument_spec).
		-ifdef (server_start_argument).
			-define (server_start_argument_spec, ?server_start_argument).
		-else.
			-define (server_start_argument_spec, any ()).
		-endif.
	-endif.
	
	-ifdef (server_start_argument_spec).
		-define (gen_server__server_start_argument_spec, ?server_start_argument_spec).
	-endif.
	
	
	-ifndef (server_state_spec).
		-ifdef (server_state).
			-define (server_state_spec, ?server_state).
		-endif.
	-endif.
	
	-ifdef (server_state_spec).
		-define (gen_server__server_state_spec, ?server_state_spec).
	-endif.
	
-endif.


% ----- eof
