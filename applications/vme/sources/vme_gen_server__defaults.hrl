
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
% ----- gen_server behaviour defaults
% ----------------------------------------------------------------------------


% ----- init

-define (gen_server__init_default (Argument, State),
		?gen_server__init_wrapper (Argument,
			?gen_server__init_ok (State))).


% ----- handle_call

-define (gen_server__handle_call_default,
		?gen_server__handle_call_wrapper (Message, Sender, State,
			case Message of
				?gen_server__stop_message (StopReason) ->
					?gen_server__call_stop_with_reply (?gen_server__ok, StopReason, State);
				_ ->
					?gen_server__call_stop_with_reply_unknown_message (Message, Sender, State)
			end)).


% ----- handle_cast

-define (gen_server__handle_cast_default,
		?gen_server__handle_cast_wrapper (Message, State,
			case Message of
				?gen_server__stop_message (StopReason) ->
					?gen_server__cast_stop_with_noreply (StopReason, State);
				_ ->
					?gen_server__cast_stop_with_noreply_unknown_message (Message, State)
			end)).


% ----- handle_info

-define (gen_server__handle_info_default,
		?gen_server__handle_info_wrapper (Message, State,
			case Message of
				?gen_server__stop_message (StopReason) ->
					?gen_server__info_stop_with_noreply (StopReason, State);
				_ ->
				?gen_server__info_stop_with_noreply_unknown_message (Message, State)
			end)).


% ----- terminate

-define (gen_server__terminate_default,
		?gen_server__terminate_wrapper (Reason, _State,
			case Reason of
				normal -> ok;
				_ -> ok
			end)).


% ----- code_change

-define (gen_server__code_change_default,
		?gen_server__code_change_wrapper (_Version, State, _Extra,
			{ok, State})).


% ----------------------------------------------------------------------------
% ----- gen_server start and start_link defaults
% ----------------------------------------------------------------------------


% ----- generic start and start_link

-define (gen_server__start_smao (Server, Module, Argument, Options),
		gen_server:start (Server, Module, Argument, Options)).

-define (gen_server__start_mao (Module, Argument, Options),
		gen_server:start (Module, Argument, Options)).

-define (gen_server__start_link_smao (Server, Module, Argument, Options),
		gen_server:start_link (Server, Module, Argument, Options)).

-define (gen_server__start_link_mao (Module, Argument, Options),
		gen_server:start_link (Module, Argument, Options)).


% ----- start (server, argument, options)

-define (gen_server__start_m_sao_default (Module),
		start (Server, Argument, Options) ->
			?gen_server__start_smao (Server, Module, Argument, Options)).

-define (gen_server__start_dm_sao_default,
		?gen_server__start_m_sao_default (
			?gen_server__module)).


% ----- start_link (server, argument, options)

-define (gen_server__start_link_m_sao_default (Module),
		start_link (Server, Argument, Options) ->
			?gen_server__start_link_smao (Server, Module, Argument, Options)).

-define (gen_server__start_link_dm_sao_default,
		?gen_server__start_link_m_sao_default (
			?gen_server__module)).


% ----- start (argument, options)

-define (gen_server__start_m_ao_default (Module),
		start (Argument, Options) ->
			?gen_server__start_mao (Module, Argument, Options)).

-define (gen_server__start_sm_ao_default (Server, Module),
		start (Argument, Options) ->
			?gen_server__start_smao (Server, Module, Argument, Options)).

-define (gen_server__start_dm_ao_default,
		?gen_server__start_m_ao_default (
			?gen_server__module)).

-define (gen_server__start_dsm_ao_default,
		?gen_server__start_sm_ao_default (
			?gen_server__server_default_start_name,
			?gen_server__module)).


% ----- start_link (argument, options)

-define (gen_server__start_link_m_ao_default (Module),
		start_link (Argument, Options) ->
			?gen_server__start_link_mao (Module, Argument, Options)).

-define (gen_server__start_link_sm_ao_default (Server, Module),
		start_link (Argument, Options) ->
			?gen_server__start_link_smao (Server, Module, Argument, Options)).

-define (gen_server__start_link_dm_ao_default,
		?gen_server__start_link_m_ao_default (
			?gen_server__module)).

-define (gen_server__start_link_dsm_ao_default,
		?gen_server__start_link_sm_ao_default (
			?gen_server__server_default_start_name,
			?gen_server__module)).


% ----- start (argument)

-define (gen_server__start_mo_a_default (Module, Options),
		start (Argument) ->
			?gen_server__start_mao (Module, Argument, Options)).

-define (gen_server__start_smo_a_default (Server, Module, Options),
		start (Argument) ->
			?gen_server__start_smao (Server, Module, Argument, Options)).

-define (gen_server__start_dmo_a_default,
		?gen_server__start_mo_a_default (
			?gen_server__module,
			?gen_server__server_default_start_options)).

-define (gen_server__start_dsmo_a_default,
		?gen_server__start_smo_a_default (
			?gen_server__server_default_start_name,
			?gen_server__module,
			?gen_server__server_default_start_options)).


% ----- start_link (argument)

-define (gen_server__start_link_mo_a_default (Module, Options),
		start_link (Argument) ->
			?gen_server__start_link_mao (Module, Argument, Options)).

-define (gen_server__start_link_smo_a_default (Server, Module, Options),
		start_link (Argument) ->
			?gen_server__start_link_smao (Server, Module, Argument, Options)).

-define (gen_server__start_link_dmo_a_default,
		?gen_server__start_link_mo_a_default (
			?gen_server__module,
			?gen_server__server_default_start_options)).

-define (gen_server__start_link_dsmo_a_default,
		?gen_server__start_link_smo_a_default (
			?gen_server__server_default_start_name,
			?gen_server__module,
			?gen_server__server_default_start_options)).


% ----- start (options)

-define (gen_server__start_ma_o_default (Module, Argument),
		start (Options) ->
			?gen_server__start_mao (Module, Argument, Options)).

-define (gen_server__start_sma_o_default (Server, Module, Argument),
		start (Options) ->
			?gen_server__start_smao (Server, Module, Argument, Options)).

-define (gen_server__start_dma_o_default,
		?gen_server__start_ma_o_default (
			?gen_server__module,
			?gen_server__server_default_start_argument)).

-define (gen_server__start_dsma_o_default,
		?gen_server__start_sma_o_default (
			?gen_server__server_default_start_name,
			?gen_server__module,
			?gen_server__server_default_start_argument)).


% ----- start_link (options)

-define (gen_server__start_link_ma_o_default (Module, Argument),
		start_link (Options) ->
			?gen_server__start_link_mao (Module, Argument, Options)).

-define (gen_server__start_link_sma_o_default (Server, Module, Argument),
		start_link (Options) ->
			?gen_server__start_link_smao (Server, Module, Argument, Options)).

-define (gen_server__start_link_dma_o_default,
		?gen_server__start_link_ma_o_default (
			?gen_server__module,
			?gen_server__server_default_start_argument)).

-define (gen_server__start_link_dsma_o_default,
		?gen_server__start_link_sma_o_default (
			?gen_server__server_default_start_name,
			?gen_server__module,
			?gen_server__server_default_start_argument)).


% ----- start ()

-define (gen_server__start_mao_default (Module, Argument, Options),
		start () ->
			?gen_server__start_mao (Module, Argument, Options)).

-define (gen_server__start_smao_default (Server, Module, Argument, Options),
		start () ->
			?gen_server__start_smao (Server, Module, Argument, Options)).

-define (gen_server__start_dmao_default,
		?gen_server__start_mao_default (
			?gen_server__module,
			?gen_server__server_default_start_argument,
			?gen_server__server_default_start_options)).

-define (gen_server__start_dsmao_default,
		?gen_server__start_smao_default (
			?gen_server__server_default_start_name,
			?gen_server__module,
			?gen_server__server_default_start_argument,
			?gen_server__server_default_start_options)).


% ----- start_link ()

-define (gen_server__start_link_mao_default (Module, Argument, Options),
		start_link () ->
			?gen_server__start_link_mao (Module, Argument, Options)).

-define (gen_server__start_link_smao_default (Server, Module, Argument, Options),
		start_link () ->
			?gen_server__start_link_smao (Server, Module, Argument, Options)).

-define (gen_server__start_link_dmao_default,
		?gen_server__start_link_mao_default (
			?gen_server__module,
			?gen_server__server_default_start_argument,
			?gen_server__server_default_start_options)).

-define (gen_server__start_link_dsmao_default,
		?gen_server__start_link_smao_default (
			?gen_server__server_default_start_name,
			?gen_server__module,
			?gen_server__server_default_start_argument,
			?gen_server__server_default_start_options)).


% ----------------------------------------------------------------------------
% ----- gen_server call and cast defaults
% ----------------------------------------------------------------------------


% ----- call and cast

-define (gen_server__call_sr (Server, Message),
		gen_server:call (Server, Message)).

-define (gen_server__cast_sr (Server, Message),
		gen_server:cast (Server, Message)).


% ----- call (server, arguments)

-define (gen_server__call_im_s0_default (Name, Interface, Method),
		Name (Server) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im0 (Interface, Method))).

-define (gen_server__call_im_s1_default (Name, Interface, Method),
		Name (Server, Argument_1) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im1 (Interface, Method, Argument_1))).

-define (gen_server__call_im_s2_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im2 (Interface, Method, Argument_1, Argument_2))).

-define (gen_server__call_im_s3_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2, Argument_3) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im3 (Interface, Method, Argument_1, Argument_2, Argument_3))).

-define (gen_server__call_im_s4_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2, Argument_3, Argument_4) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im4 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4))).

-define (gen_server__call_im_s5_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im5 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5))).

-define (gen_server__call_dim_s0_default (Name),
		?gen_server__call_im_s0_default (Name, ?gen_server__module, Name)).

-define (gen_server__call_dim_s1_default (Name),
		?gen_server__call_im_s1_default (Name, ?gen_server__module, Name)).

-define (gen_server__call_dim_s2_default (Name),
		?gen_server__call_im_s2_default (Name, ?gen_server__module, Name)).

-define (gen_server__call_dim_s3_default (Name),
		?gen_server__call_im_s3_default (Name, ?gen_server__module, Name)).

-define (gen_server__call_dim_s4_default (Name),
		?gen_server__call_im_s4_default (Name, ?gen_server__module, Name)).

-define (gen_server__call_dim_s5_default (Name),
		?gen_server__call_im_s5_default (Name, ?gen_server__module, Name)).


% ----- cast (server, arguments)

-define (gen_server__cast_im_s0_default (Name, Interface, Method),
		Name (Server) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im0 (Interface, Method))).

-define (gen_server__cast_im_s1_default (Name, Interface, Method),
		Name (Server, Argument_1) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im1 (Interface, Method, Argument_1))).

-define (gen_server__cast_im_s2_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im2 (Interface, Method, Argument_1, Argument_2))).

-define (gen_server__cast_im_s3_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2, Argument_3) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im3 (Interface, Method, Argument_1, Argument_2, Argument_3))).

-define (gen_server__cast_im_s4_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2, Argument_3, Argument_4) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im4 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4))).

-define (gen_server__cast_im_s5_default (Name, Interface, Method),
		Name (Server, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im5 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5))).

-define (gen_server__cast_dim_s0_default (Name),
		?gen_server__cast_im_s0_default (Name, ?gen_server__module, Name)).

-define (gen_server__cast_dim_s1_default (Name),
		?gen_server__cast_im_s1_default (Name, ?gen_server__module, Name)).

-define (gen_server__cast_dim_s2_default (Name),
		?gen_server__cast_im_s2_default (Name, ?gen_server__module, Name)).

-define (gen_server__cast_dim_s3_default (Name),
		?gen_server__cast_im_s3_default (Name, ?gen_server__module, Name)).

-define (gen_server__cast_dim_s4_default (Name),
		?gen_server__cast_im_s4_default (Name, ?gen_server__module, Name)).

-define (gen_server__cast_dim_s5_default (Name),
		?gen_server__cast_im_s5_default (Name, ?gen_server__module, Name)).


% ----- call (arguments)

-define (gen_server__call_sim_0_default (Name, Server, Interface, Method),
		Name () ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im0 (Interface, Method))).

-define (gen_server__call_sim_1_default (Name, Server, Interface, Method),
		Name (Argument_1) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im1 (Interface, Method, Argument_1))).

-define (gen_server__call_sim_2_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im2 (Interface, Method, Argument_1, Argument_2))).

-define (gen_server__call_sim_3_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2, Argument_3) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im3 (Interface, Method, Argument_1, Argument_2, Argument_3))).

-define (gen_server__call_sim_4_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2, Argument_3, Argument_4) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im4 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4))).

-define (gen_server__call_sim_5_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2, Argument_3, Argument_4, Argument_5) ->
			?gen_server__call_sr (Server,
				?gen_server__call_message_im5 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5))).

-define (gen_server__call_dsim_0_default (Name),
		?gen_server__call_sim_0_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__call_dsim_1_default (Name),
		?gen_server__call_sim_1_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__call_dsim_2_default (Name),
		?gen_server__call_sim_2_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__call_dsim_3_default (Name),
		?gen_server__call_sim_3_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__call_dsim_4_default (Name),
		?gen_server__call_sim_4_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__call_dsim_5_default (Name),
		?gen_server__call_sim_5_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).


% ----- cast (arguments)

-define (gen_server__cast_sim_0_default (Name, Server, Interface, Method),
		Name () ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im0 (Interface, Method))).

-define (gen_server__cast_sim_1_default (Name, Server, Interface, Method),
		Name (Argument_1) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im1 (Interface, Method, Argument_1))).

-define (gen_server__cast_sim_2_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im2 (Interface, Method, Argument_1, Argument_2))).

-define (gen_server__cast_sim_3_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2, Argument_3) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im3 (Interface, Method, Argument_1, Argument_2, Argument_3))).

-define (gen_server__cast_sim_4_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2, Argument_3, Argument_4) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im4 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4))).

-define (gen_server__cast_sim_5_default (Name, Server, Interface, Method),
		Name (Argument_1, Argument_2, Argument_3, Argument_4, Argument_5) ->
			?gen_server__cast_sr (Server,
				?gen_server__cast_message_im5 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5))).

-define (gen_server__cast_dsim_0_default (Name),
		?gen_server__cast_sim_0_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__cast_dsim_1_default (Name),
		?gen_server__cast_sim_1_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__cast_dsim_2_default (Name),
		?gen_server__cast_sim_2_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__cast_dsim_3_default (Name),
		?gen_server__cast_sim_3_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__cast_dsim_4_default (Name),
		?gen_server__cast_sim_4_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).

-define (gen_server__cast_dsim_5_default (Name),
		?gen_server__cast_sim_5_default (Name, ?gen_server__server_default_send_name, ?gen_server__module, Name)).


% ----------------------------------------------------------------------------
% ----- gen_server stop defaults
% ----------------------------------------------------------------------------


% ----- stop

-define (gen_server__stop_sr (Server, StopReason),
		gen_server:call (Server,
			?gen_server__stop_message (StopReason))).


% ----- stop (Server, StopReason)

-define (gen_server__stop_sr_default,
		stop (Server, StopReason) ->
			?gen_server__stop_sr (Server, StopReason)).


% ----- stop (Server)

-define (gen_server__stop_dr_s_default,
		stop (Server) ->
			?gen_server__stop_sr (Server,
				?gen_server__server_default_stop_reason)).


% ----- stop (StopReason)

-define (gen_server__stop_ds_r_default,
		stop (StopReason) ->
			?gen_server__stop_sr (
				?gen_server__server_default_send_name, StopReason)).


% ----- stop ()

-define (gen_server__stop_dsr_default,
		stop () ->
			?gen_server__stop_sr (
				?gen_server__server_default_send_name,
				?gen_server__server_default_stop_reason)).


% ----------------------------------------------------------------------------
% ----- gen_server defaults interface aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_interface_aliases).
	
	
	% ----- call and cast aliases
	
	-define (call_sr (A1, A2), ?gen_server__call_sr (A1, A2)).
	
	-define (cast_sr (A1, A2), ?gen_server__cast_sr (A1, A2)).
	
	-define (call_im_s0_default (A1, A2, A3), ?gen_server__call_im_s0_default (A1, A2, A3)).
	-define (call_sim_0_default (A1, A2, A3, A4), ?gen_server__call_sim_0_default (A1, A2, A3, A4)).
	-define (call_dim_s0_default (A1), ?gen_server__call_dim_s0_default (A1)).
	-define (call_dsim_0_default (A1), ?gen_server__call_dsim_0_default (A1)).
	
	-define (cast_im_s0_default (A1, A2, A3), ?gen_server__cast_im_s0_default (A1, A2, A3)).
	-define (cast_sim_0_default (A1, A2, A3, A4), ?gen_server__cast_sim_0_default (A1, A2, A3, A4)).
	-define (cast_dim_s0_default (A1), ?gen_server__cast_dim_s0_default (A1)).
	-define (cast_dsim_0_default (A1), ?gen_server__cast_dsim_0_default (A1)).
	
	-define (call_im_s1_default (A1, A2, A3), ?gen_server__call_im_s1_default (A1, A2, A3)).
	-define (call_sim_1_default (A1, A2, A3, A4), ?gen_server__call_sim_1_default (A1, A2, A3, A4)).
	-define (call_dim_s1_default (A1), ?gen_server__call_dim_s1_default (A1)).
	-define (call_dsim_1_default (A1), ?gen_server__call_dsim_1_default (A1)).
	
	-define (cast_im_s1_default (A1, A2, A3), ?gen_server__cast_im_s1_default (A1, A2, A3)).
	-define (cast_sim_1_default (A1, A2, A3, A4), ?gen_server__cast_sim_1_default (A1, A2, A3, A4)).
	-define (cast_dim_s1_default (A1), ?gen_server__cast_dim_s1_default (A1)).
	-define (cast_dsim_1_default (A1), ?gen_server__cast_dsim_1_default (A1)).
	
	-define (call_im_s2_default (A1, A2, A3), ?gen_server__call_im_s2_default (A1, A2, A3)).
	-define (call_sim_2_default (A1, A2, A3, A4), ?gen_server__call_sim_2_default (A1, A2, A3, A4)).
	-define (call_dim_s2_default (A1), ?gen_server__call_dim_s2_default (A1)).
	-define (call_dsim_2_default (A1), ?gen_server__call_dsim_2_default (A1)).
	
	-define (cast_im_s2_default (A1, A2, A3), ?gen_server__cast_im_s2_default (A1, A2, A3)).
	-define (cast_sim_2_default (A1, A2, A3, A4), ?gen_server__cast_sim_2_default (A1, A2, A3, A4)).
	-define (cast_dim_s2_default (A1), ?gen_server__cast_dim_s2_default (A1)).
	-define (cast_dsim_2_default (A1), ?gen_server__cast_dsim_2_default (A1)).
	
	-define (call_im_s3_default (A1, A2, A3), ?gen_server__call_im_s3_default (A1, A2, A3)).
	-define (call_sim_3_default (A1, A2, A3, A4), ?gen_server__call_sim_3_default (A1, A2, A3, A4)).
	-define (call_dim_s3_default (A1), ?gen_server__call_dim_s3_default (A1)).
	-define (call_dsim_3_default (A1), ?gen_server__call_dsim_3_default (A1)).
	
	-define (cast_im_s3_default (A1, A2, A3), ?gen_server__cast_im_s3_default (A1, A2, A3)).
	-define (cast_sim_3_default (A1, A2, A3, A4), ?gen_server__cast_sim_3_default (A1, A2, A3, A4)).
	-define (cast_dim_s3_default (A1), ?gen_server__cast_dim_s3_default (A1)).
	-define (cast_dsim_3_default (A1), ?gen_server__cast_dsim_3_default (A1)).
	
	-define (call_im_s4_default (A1, A2, A3), ?gen_server__call_im_s4_default (A1, A2, A3)).
	-define (call_sim_4_default (A1, A2, A3, A4), ?gen_server__call_sim_4_default (A1, A2, A3, A4)).
	-define (call_dim_s4_default (A1), ?gen_server__call_dim_s4_default (A1)).
	-define (call_dsim_4_default (A1), ?gen_server__call_dsim_4_default (A1)).
	
	-define (cast_im_s4_default (A1, A2, A3), ?gen_server__cast_im_s4_default (A1, A2, A3)).
	-define (cast_sim_4_default (A1, A2, A3, A4), ?gen_server__cast_sim_4_default (A1, A2, A3, A4)).
	-define (cast_dim_s4_default (A1), ?gen_server__cast_dim_s4_default (A1)).
	-define (cast_dsim_4_default (A1), ?gen_server__cast_dsim_4_default (A1)).
	
	-define (call_im_s5_default (A1, A2, A3), ?gen_server__call_im_s5_default (A1, A2, A3)).
	-define (call_sim_5_default (A1, A2, A3, A4), ?gen_server__call_sim_5_default (A1, A2, A3, A4)).
	-define (call_dim_s5_default (A1), ?gen_server__call_dim_s5_default (A1)).
	-define (call_dsim_5_default (A1), ?gen_server__call_dsim_5_default (A1)).
	
	-define (cast_im_s5_default (A1, A2, A3), ?gen_server__cast_im_s5_default (A1, A2, A3)).
	-define (cast_sim_5_default (A1, A2, A3, A4), ?gen_server__cast_sim_5_default (A1, A2, A3, A4)).
	-define (cast_dim_s5_default (A1), ?gen_server__cast_dim_s5_default (A1)).
	-define (cast_dsim_5_default (A1), ?gen_server__cast_dsim_5_default (A1)).
	
	
	% ----- stop aliases
	
	-define (stop_sr_default, ?gen_server__stop_sr_default).
	-define (stop_ds_r_default, ?gen_server__stop_ds_r_default).
	-define (stop_dr_s_default, ?gen_server__stop_dr_s_default).
	-define (stop_dsr_default, ?gen_server__stop_dsr_default).
	
-endif.


% ----------------------------------------------------------------------------
% ----- gen_server defaults internal aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_internal_aliases).
	
	
	% ----- handler aliases
	
	-define (init_default (A1, A2), ?gen_server__init_default (A1, A2)).
	-define (handle_call_default, ?gen_server__handle_call_default).
	-define (handle_cast_default, ?gen_server__handle_cast_default).
	-define (handle_info_default, ?gen_server__handle_info_default).
	-define (terminate_default, ?gen_server__terminate_default).
	-define (code_change_default, ?gen_server__code_change_default).
	
	
	% ----- start and start_link aliases
	
	-define (start_mao (A1, A2, A3), ?gen_server__start_mao (A1, A2, A3)).
	-define (start_smao (A1, A2, A3, A4), ?gen_server__start_smao (A1, A2, A3, A4)).
	
	-define (start_m_sao_default (A1), ?gen_server__start_m_sao_default (A1)).
	-define (start_dm_sao_default, ?gen_server__start_dm_sao_default).
	
	-define (start_link_m_sao_default (A1), ?gen_server__start_link_m_sao_default (A1)).
	-define (start_link_dm_sao_default, ?gen_server__start_link_dm_sao_default).
	
	-define (start_m_ao_default (A1), ?gen_server__start_m_ao_default (A1)).
	-define (start_sm_ao_default (A1, A2), ?gen_server__start_sm_ao_default (A1, A2)).
	-define (start_dm_ao_default, ?gen_server__start_dm_ao_default).
	-define (start_dsm_ao_default, ?gen_server__start_dsm_ao_default).
	
	-define (start_link_m_ao_default (A1), ?gen_server__start_link_m_ao_default (A1)).
	-define (start_link_sm_ao_default (A1, A2), ?gen_server__start_link_sm_ao_default (A1, A2)).
	-define (start_link_dm_ao_default, ?gen_server__start_link_dm_ao_default).
	-define (start_link_dsm_ao_default, ?gen_server__start_link_dsm_ao_default).
	
	-define (start_mo_a_default (A1, A2), ?gen_server__start_mo_a_default (A1, A2)).
	-define (start_smo_a_default (A1, A2, A3), ?gen_server__start_smo_a_default (A1, A2, A3)).
	-define (start_dmo_a_default, ?gen_server__start_dmo_a_default).
	-define (start_dsmo_a_default, ?gen_server__start_dsmo_a_default).
	
	-define (start_link_mo_a_default (A1, A2), ?gen_server__start_link_mo_a_default (A1, A2)).
	-define (start_link_smo_a_default (A1, A2, A3), ?gen_server__start_link_smo_a_default (A1, A2, A3)).
	-define (start_link_dmo_a_default, ?gen_server__start_link_dmo_a_default).
	-define (start_link_dsmo_a_default, ?gen_server__start_link_dsmo_a_default).
	
	-define (start_ma_o_default (A1, A2), ?gen_server__start_mo_a_default (A1, A2)).
	-define (start_sma_o_default (A1, A2, A3), ?gen_server__start_smo_a_default (A1, A2, A3)).
	-define (start_dma_o_default, ?gen_server__start_dma_o_default).
	-define (start_dsma_o_default, ?gen_server__start_dsma_o_default).
	
	-define (start_link_ma_o_default (A1, A2), ?gen_server__start_link_ma_o_default (A1, A2)).
	-define (start_link_sma_o_default (A1, A2, A3), ?gen_server__start_link_sma_o_default (A1, A2, A3)).
	-define (start_link_dma_o_default, ?gen_server__start_link_dma_o_default).
	-define (start_link_dsma_o_default, ?gen_server__start_link_dsma_o_default).
	
	-define (start_mao_default (A1, A2, A3), ?gen_server__start_mao_default (A1, A2, A3)).
	-define (start_smao_default (A1, A2, A3, A4), ?gen_server__start_smao_default (A1, A2, A3, A4)).
	-define (start_dmao_default, ?gen_server__start_dmao_default).
	-define (start_dsmao_default, ?gen_server__start_dsmao_default).
	
	-define (start_link_mao_default (A1, A2, A3), ?gen_server__start_link_mao_default (A1, A2, A3)).
	-define (start_link_smao_default (A1, A2, A3, A4), ?gen_server__start_link_smao_default (A1, A2, A3, A4)).
	-define (start_link_dmao_default, ?gen_server__start_link_dmao_default).
	-define (start_link_dsmao_default, ?gen_server__start_link_dsmao_default).
	
-endif.


% ----- eof
