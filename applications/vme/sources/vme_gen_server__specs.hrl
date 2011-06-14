
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
% ----- gen_server behaviour specs
% ----------------------------------------------------------------------------

% ----- init

-type (gen_server__init_ok (State) ::
		{ok, State}).

-type (gen_server__init_stop (Stop) ::
		{stop, Stop}).

-type (gen_server__init_stop_with_error (Error) ::
		?gen_server__init_stop (
			?gen_server__error (Error))).

-type (gen_server__init_ok_or_stop_with_error (Error, State) ::
		gen_server__init_ok (State) |
		gen_server__init_stop_with_error (Error)).

-type (gen_server__init_outcomes (Stop, State) ::
		gen_server__init_ok (State) |
		gen_server__init_stop (Stop) |
		{ok, State, timeout () | hibernate} |
		ignore).

-define (gen_server__init_ok_spec (Argument, State),
		-spec (
			init (Argument) ->
				gen_server__init_ok (State))).

-define (gen_server__init_ok_or_stop_with_error_spec (Argument, Error, State),
		-spec (
			init (Argument) ->
				gen_server__init_ok_or_stop_with_error (Error, State))).

-define (gen_server__init_default_spec (Argument, State),
		-spec (
			init (Argument) ->
				gen_server__init_ok (State))).


% ----- handle_call

-type (gen_server__handle_call_reply (Reply, State) ::
		{reply, Reply, State}).

-type (gen_server__handle_call_noreply (State) ::
		{noreply, State}).

-type (gen_server__handle_call_stop_with_reply (StopReply, StopReason, State) ::
		{stop, StopReason, StopReply, State}).

-type (gen_server__handle_call_stop_with_noreply (StopReason, State) ::
		{stop, StopReason, State}).

-type (gen_server__handle_call_reply_or_stop_with_reply (Reply, StopReply, StopReason, State) ::
		gen_server__handle_call_reply (Reply, State) |
		gen_server__handle_call_stop_with_reply (StopReply, StopReason, State)).

-type (gen_server__handle_call_outcomes (Reply, StopReply, StopReason, State) ::
		gen_server__handle_call_reply (Reply, State) |
		gen_server__handle_call_noreply (State) |
		gen_server__handle_call_stop_with_reply (StopReply, StopReason, State) |
		gen_server__handle_call_stop_with_noreply (StopReason, State) |
		{reply, Reply, State, timeout () | hibernate} |
		{noreply, State, timeout () | hibernate}).

-define (gen_server__handle_call_reply_spec (Message, Reply, State),
		-spec (
			handle_call (Message, _Sender, State) ->
				gen_server__handle_call_reply (Reply, State))).

-define (gen_server__handle_call_reply_or_stop_with_reply_spec (Message, Reply, StopReply, StopReason, State),
		-spec (
			handle_call (Message, _Sender, State) ->
				gen_server__handle_call_reply_or_stop_with_reply (Reply, StopReply, StopReason, State))).

-define (gen_server__handle_call_default_spec,
		-spec (
			handle_call (Message, Sender, State) ->
				gen_server__handle_call_stop_with_reply (
					?gen_server__unknown_message (Message, Sender),
					?gen_server__unknown_message (Message, Sender),
					State))).


% ----- handle_cast

-type (gen_server__handle_cast_noreply (State) ::
		{noreply, State}).

-type (gen_server__handle_cast_stop_with_noreply (StopReason, State) ::
		{stop, StopReason, State}).

-type (gen_server__handle_cast_noreply_or_stop_with_noreply (StopReason, State) ::
		gen_server__handle_cast_noreply (State) |
		gen_server__handle_cast_stop_with_noreply (StopReason, State)).

-type (gen_server__handle_cast_outcomes (StopReason, State) ::
		gen_server__handle_cast_noreply (State) |
		gen_server__handle_cast_stop_with_noreply (StopReason, State) |
		{noreply, State, timeout() | hibernate}).

-define (gen_server__handle_cast_noreply_spec (Message, State),
		-spec (
			handle_cast (Message, State) ->
				gen_server__handle_cast_noreply (State))).

-define (gen_server__handle_cast_noreply_or_stop_with_noreply_spec (Message, StopReason, State),
		-spec (
			handle_cast (Message, State) ->
				gen_server__handle_cast_noreply_or_stop_with_noreply (StopReason, State))).

-define (gen_server__handle_cast_default_spec,
		-spec (
			handle_cast (Message, State) ->
				gen_server__handle_cast_stop_with_noreply (
					?gen_server__unknown_message (Message, undefined),
					State))).


% ----- handle_info

-type (gen_server__handle_info_noreply (State) ::
		{noreply, State}).

-type (gen_server__handle_info_stop_with_noreply (StopReason, State) ::
		{stop, StopReason, State}).

-type (gen_server__handle_info_noreply_or_stop_with_noreply (StopReason, State) ::
		gen_server__handle_info_noreply (State) |
		gen_server__handle_info_stop_with_noreply (StopReason, State)).

-type (gen_server__handle_info_outcomes (StopReason, State) ::
		gen_server__handle_info_noreply (State) |
		gen_server__handle_info_stop_with_noreply (StopReason, State) |
		{noreply, State, timeout() | hibernate}).

-define (gen_server__handle_info_noreply_spec (Message, State),
		-spec (
			handle_info (Message, State) ->
				gen_server__handle_info_noreply (State))).

-define (gen_server__handle_info_noreply_or_stop_with_noreply_spec (Message, StopReason, State),
		-spec (
			handle_info (Message, State) ->
				gen_server__handle_info_noreply_or_stop_with_noreply (StopReason, State))).

-define (gen_server__handle_info_default_spec,
		-spec (
			handle_info (Message, State) ->
				gen_server__handle_info_stop_with_noreply (
					?gen_server__unknown_message (Message, undefined),
					State))).


% ----- terminate

-type (gen_server__terminate_ok () ::
		ok).

-type (gen_server__terminate_outcomes () ::
		gen_server__terminate_ok ()).

-define (gen_server__terminate_spec (StopReason, State),
		-spec (
			terminate (StopReason, State) ->
				gen_server__terminate_outcomes ())).

-define (gen_server__terminate_default_spec,
		-spec (
			terminate (_StopReason, _State) ->
				gen_server__terminate_ok ())).


% ----- code_change

-type (gen_server__code_change_version (Version) ::
		Version |
		{down, Version}).

-type (gen_server__code_change_ok (State) ::
		{ok, State}).

-type (gen_server__code_change_outcomes (State) ::
		gen_server__code_change_ok (State)).

-define (gen_server__code_change_spec (Version, Extra, State),
		-spec (
			code_change (Version, State, Extra) ->
				gen_server__code_change_outcomes (State))).

-define (gen_server__code_change_default_spec,
		-spec (
			code_change (_Version, State, _Extra) ->
				gen_server__code_change_ok (State))).


% ----------------------------------------------------------------------------
% ----- gen_server start and start_link specs
% ----------------------------------------------------------------------------

% ----- start and start_link options

-type (gen_server__start_options () ::
		[
			{debug,[debug | log | statistics | trace | {any (), any ()}]} |
			{spawn_opt, [link | {any (), any ()}]} |
			{timeout, infinity | non_neg_integer ()}]).


% ----- start and start_link (server, argument, options)

-define (gen_server__start_sao_spec (Argument),
		-spec (
			start (gen__start_name (), Argument, gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_link_sao_spec (Argument),
		-spec (
			start_link (gen__start_name (), Argument, gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_sao_default_spec,
		?gen_server__start_sao_spec (
			?gen_server__server_start_argument_spec)).

-define (gen_server__start_link_sao_default_spec,
		?gen_server__start_link_sao_spec (
			?gen_server__server_start_argument_spec)).


% ----- start and start_link (server)

-define (gen_server__start_s_spec,
		-spec (
			start (gen__start_name ()) ->
				gen__start_outcome ())).

-define (gen_server__start_link_s_spec,
		-spec (
			start_link (gen__start_name ()) ->
				gen__start_outcome ())).

-define (gen_server__start_s_default_spec,
		?gen_server__start_s_spec).

-define (gen_server__start_link_s_default_spec,
		?gen_server__start_link_s_spec).


% ----- start and start_link (argument)

-define (gen_server__start_a_spec (Argument),
		-spec (
			start (Argument) ->
				gen__start_outcome ())).

-define (gen_server__start_link_a_spec (Argument),
		-spec (
			start_link (Argument) ->
				gen__start_outcome ())).

-define (gen_server__start_a_default_spec,
		?gen_server__start_a_spec (
			?gen_server__server_start_argument_spec)).

-define (gen_server__start_link_a_default_spec,
		?gen_server__start_link_a_spec (
			?gen_server__server_start_argument_spec)).


% ----- start and start_link (options)

-define (gen_server__start_o_spec,
		-spec (
			start (gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_link_o_spec,
		-spec (
			start_link (gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_o_default_spec,
		?gen_server__start_o_spec).

-define (gen_server__start_link_o_default_spec,
		?gen_server__start_link_o_spec).


% ----- start and start_link (server, argument)

-define (gen_server__start_sa_spec (Argument),
		-spec (
			start (gen__start_name (), Argument) ->
				gen__start_outcome ())).

-define (gen_server__start_link_sa_spec (Argument),
		-spec (
			start_link (gen__start_name (), Argument) ->
				gen__start_outcome ())).

-define (gen_server__start_sa_default_spec,
		?gen_server__start_sa_spec (
			?gen_server__server_start_argument_spec)).

-define (gen_server__start_link_sa_default_spec,
		?gen_server__start_link_sa_spec (
			?gen_server__server_start_argument_spec)).


% ----- start and start_link (server, options)

-define (gen_server__start_so_spec,
		-spec (
			start (gen__start_name (), gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_link_so_spec,
		-spec (
			start_link (gen__start_name (), gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_so_default_spec,
		?gen_server__start_so_spec).

-define (gen_server__start_link_so_default_spec,
		?gen_server__start_link_so_spec).


% ----- start and start_link (argument, options)

-define (gen_server__start_ao_spec (Argument),
		-spec (
			start (Argument, gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_link_ao_spec (Argument),
		-spec (
			start_link (Argument, gen_server__start_options ()) ->
				gen__start_outcome ())).

-define (gen_server__start_ao_default_spec,
		?gen_server__start_ao_spec (
			?gen_server__server_start_argument_spec)).

-define (gen_server__start_link_ao_default_spec,
		?gen_server__start_link_ao_spec (
			?gen_server__server_start_argument_spec)).


% ---- start and start_link ()

-define (gen_server__start_spec,
		-spec (
			start () ->
				gen__start_outcome ())).

-define (gen_server__start_link_spec,
		-spec (
			start_link () ->
				gen__start_outcome ())).

-define (gen_server__start_default_spec,
		?gen_server__start_spec).

-define (gen_server__start_link_default_spec,
		?gen_server__start_link_spec).


% ----------------------------------------------------------------------------
% ----- gen_server stop specs
% ----------------------------------------------------------------------------

% ---- stop (server, reason)

-define (gen_server__stop_sr_spec (StopReason),
		-spec (
			stop (gen__send_name (), StopReason) ->
				gen__stop_outcome ())).

-define (gen_server__stop_sr_default_spec,
		?gen_server__stop_sr_spec (
			?gen_server__server_default_stop_reason)).


% ---- stop (server)

-define (gen_server__stop_s_spec,
		-spec (
			stop (gen__send_name ()) ->
				gen__stop_outcome ())).

-define (gen_server__stop_s_default_spec,
		?gen_server__stop_s_spec).


% ---- stop (reason)

-define (gen_server__stop_r_spec (StopReason),
		-spec (
			stop (StopReason) ->
				gen__stop_outcome ())).

-define (gen_server__stop_r_default_spec,
		?gen_server__stop_r_spec (
			?gen_server__server_default_stop_reason)).


% ---- stop ()

-define (gen_server__stop_spec,
		-spec (
			stop () ->
				gen__stop_outcome ())).

-define (gen_server__stop_default_spec,
		?gen_server__stop_spec).


% ----------------------------------------------------------------------------
% ----- gen_server spec internal aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_internal_aliases).
	
	-type (init_ok (A1) :: gen_server__init_ok (A1)).
	-type (init_stop (A1) :: gen_server__init_stop (A1)).
	-type (init_stop_with_error (A1) :: gen_server__init_stop_with_error (A1)).
	-type (init_ok_or_stop_with_error (A1, A2) :: gen_server__init_ok_or_stop_with_error (A1, A2)).
	-define (init_ok_spec (A1, A2), ?gen_server__init_ok_spec (A1, A2)).
	-define (init_ok_or_stop_with_error_spec (A1, A2, A3), ?gen_server__init_ok_or_stop_with_error_spec (A1, A2, A3)).
	-define (init_default_spec (A1, A2), ?gen_server__init_default_spec (A1, A2)).
	
	-type (handle_call_reply (A1, A2) :: gen_server__handle_call_reply (A1, A2)).
	-type (handle_call_noreply (A1) :: gen_server__handle_call_noreply (A1)).
	-type (handle_call_stop_with_reply (A1, A2, A3) :: gen_server__handle_call_stop_with_reply (A1, A2, A3)).
	-type (handle_call_stop_with_noreply (A1, A2) :: gen_server__handle_call_stop_with_noreply (A1, A2)).
	-type (handle_call_reply_or_stop_with_reply (A1, A2, A3, A4) :: gen_server__handle_call_reply_or_stop_with_reply (A1, A2, A3, A4)).
	-define (handle_call_reply_spec (A1, A2, A3), ?gen_server__handle_call_reply_spec (A1, A2, A3)).
	-define (handle_call_reply_or_stop_with_reply_spec (A1, A2, A3, A4, A5), ?gen_server__handle_call_reply_or_stop_with_reply_spec (A1, A2, A3, A4, A5)).
	-define (handle_call_default_spec, ?gen_server__handle_call_default_spec).
	
	-type (handle_cast_noreply (A1) :: gen_server__handle_cast_noreply (A1)).
	-type (handle_cast_stop_with_noreply (A1, A2) :: gen_server__handle_cast_stop_with_noreply (A1, A2)).
	-type (handle_cast_noreply_or_stop_with_noreply (A1, A2) :: gen_server__handle_cast_noreply_or_stop_with_noreply (A1, A2)).
	-define (handle_cast_noreply_spec (A1, A2), ?gen_server__handle_cast_noreply_spec (A1, A2)).
	-define (handle_cast_noreply_or_stop_with_noreply_spec (A1, A2, A3), ?gen_server__handle_cast_noreply_or_stop_with_noreply_spec (A1, A2, A3)).
	-define (handle_cast_default_spec, ?gen_server__handle_cast_default_spec).
	
	-type (handle_info_noreply (A1) :: gen_server__handle_info_noreply (A1)).
	-type (handle_info_stop_with_noreply (A1, A2) :: gen_server__handle_info_stop_with_noreply (A1, A2)).
	-type (handle_info_noreply_or_stop_with_noreply (A1, A2) :: gen_server__handle_info_noreply_or_stop_with_noreply (A1, A2)).
	-define (handle_info_noreply_spec (A1, A2), ?gen_server__handle_info_noreply_spec (A1, A2)).
	-define (handle_info_noreply_or_stop_with_noreply_spec (A1, A2, A3), ?gen_server__handle_info_noreply_or_stop_with_noreply_spec (A1, A2, A3)).
	-define (handle_info_default_spec, ?gen_server__handle_info_default_spec).
	
	-type (terminate_ok () :: gen_server__terminate_ok ()).
	-define (terminate_spec (A1, A2), ?gen_server__terminate_spec (A1, A2)).
	-define (terminate_default_spec, ?gen_server__terminate_default_spec).
	
	-type (code_change_version (A1) :: gen_server__code_change_version (A1)).
	-type (code_change_ok (A1) :: gen_server__code_change_ok (A1)).
	-define (code_change_spec (A1, A2, A3), ?gen_server__code_change_spec (A1, A2, A3)).
	-define (code_change_default_spec, ?gen_server__code_change_default_spec).
	
	-define (start_sao_spec (A1), ?gen_server__start_sao_spec  (A1)).
	-define (start_link_sao_spec (A1), ?gen_server__start_link_sao_spec (A1)).
	-define (start_sao_default_spec, ?gen_server__start_sao_default_spec).
	-define (start_link_sao_default_spec, ?gen_server__start_link_sao_default_spec).
	
	-define (start_s_spec, ?gen_server__start_s_spec).
	-define (start_link_s_spec, ?gen_server__start_link_s_spec).
	-define (start_s_default_spec, ?gen_server__start_s_default_spec).
	-define (start_link_s_default_spec, ?gen_server__start_link_s_default_spec).
	
	-define (start_a_spec (A1), ?gen_server__start_a_spec (A1)).
	-define (start_link_a_spec (A1), ?gen_server__start_link_a_spec (A1)).
	-define (start_a_default_spec, ?gen_server__start_a_default_spec).
	-define (start_link_a_default_spec, ?gen_server__start_link_a_default_spec).
	
	-define (start_o_spec, ?gen_server__start_o_spec).
	-define (start_link_o_spec, ?gen_server__start_link_o_spec).
	-define (start_o_default_spec, ?gen_server__start_o_default_spec).
	-define (start_link_o_default_spec, ?gen_server__start_link_o_default_spec).
	
	-define (start_sa_spec (A1), ?gen_server__start_sa_spec (A1)).
	-define (start_link_sa_spec (A1), ?gen_server__start_link_sa_spec (A1)).
	-define (start_sa_default_spec, ?gen_server__start_sa_default_spec).
	-define (start_link_sa_default_spec, ?gen_server__start_link_sa_default_spec).
	
	-define (start_so_spec, ?gen_server__start_so_spec).
	-define (start_link_so_spec, ?gen_server__start_link_so_spec).
	-define (start_so_default_spec, ?gen_server__start_so_default_spec).
	-define (start_link_so_default_spec, ?gen_server__start_link_so_default_spec).
	
	-define (start_ao_spec (A1), ?gen_server__start_ao_spec (A1)).
	-define (start_link_ao_spec (A1), ?gen_server__start_link_ao_spec (A1)).
	-define (start_ao_default_spec, ?gen_server__start_ao_default_spec).
	-define (start_link_ao_default_spec, ?gen_server__start_link_ao_default_spec).
	
	-define (start_spec, ?gen_server__start_spec).
	-define (start_link_spec, ?gen_server__start_link_spec).
	-define (start_default_spec, ?gen_server__start_default_spec).
	-define (start_link_default_spec, ?gen_server__start_link_default_spec).
	
	-define (stop_sr_spec, ?gen_server__stop_sr_spec).
	-define (stop_sr_default_spec, ?gen_server__stop_sr_default_spec).
	
	-define (stop_s_spec, ?gen_server__stop_s_spec).
	-define (stop_s_default_spec, ?gen_server__stop_s_default_spec).
	
	-define (stop_r_spec, ?gen_server__stop_r_spec).
	-define (stop_r_default_spec, ?gen_server__stop_r_default_spec).
	
	-define (stop_spec, ?gen_server__stop_spec).
	-define (stop_default_spec, ?gen_server__stop_default_spec).
	
-endif.


% ----- eof
