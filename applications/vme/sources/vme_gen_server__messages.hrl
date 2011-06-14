
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
% ----- gen_server message
% ----------------------------------------------------------------------------


% ----- stop

-define (gen_server__stop_message (StopReason),
		{stop, StopReason}).


% ----- call

-define (gen_server__call_message_im0 (Interface, Method),
		{Interface, Method}).

-define (gen_server__call_message_im1 (Interface, Method, Argument_1),
		{Interface, Method, Argument_1}).

-define (gen_server__call_message_im2 (Interface, Method, Argument_1, Argument_2),
		{Interface, Method, Argument_1, Argument_2}).

-define (gen_server__call_message_im3 (Interface, Method, Argument_1, Argument_2, Argument_3),
		{Interface, Method, Argument_1, Argument_2, Argument_3}).

-define (gen_server__call_message_im4 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4),
		{Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4}).

-define (gen_server__call_message_im5 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5),
		{Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5}).

-define (gen_server__call_message_di_m0 (Method),
		?gen_server__call_message_im0 (?gen_server__module, Method)).

-define (gen_server__call_message_di_m1 (Method, Argument_1),
		?gen_server__call_message_im1 (?gen_server__module, Method, Argument_1)).

-define (gen_server__call_message_di_m2 (Method, Argument_1, Argument_2),
		?gen_server__call_message_im2 (?gen_server__module, Method, Argument_1, Argument_2)).

-define (gen_server__call_message_di_m3 (Method, Argument_1, Argument_2, Argument_3),
		?gen_server__call_message_im3 (?gen_server__module, Method, Argument_1, Argument_2, Argument_3)).

-define (gen_server__call_message_di_m4 (Method, Argument_1, Argument_2, Argument_3, Argument_4),
		?gen_server__call_message_im4 (?gen_server__module, Method, Argument_1, Argument_2, Argument_3, Argument_4)).

-define (gen_server__call_message_di_m5 (Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5),
		?gen_server__call_message_im5 (?gen_server__module, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5)).


% ----- cast

-define (gen_server__cast_message_im0 (Interface, Method),
		{Interface, Method}).

-define (gen_server__cast_message_im1 (Interface, Method, Argument_1),
		{Interface, Method, Argument_1}).

-define (gen_server__cast_message_im2 (Interface, Method, Argument_1, Argument_2),
		{Interface, Method, Argument_1, Argument_2}).

-define (gen_server__cast_message_im3 (Interface, Method, Argument_1, Argument_2, Argument_3),
		{Interface, Method, Argument_1, Argument_2, Argument_3}).

-define (gen_server__cast_message_im4 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4),
		{Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4}).

-define (gen_server__cast_message_im5 (Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5),
		{Interface, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5}).

-define (gen_server__cast_message_di_m0 (Method),
		?gen_server__cast_message_im0 (?gen_server__module, Method)).

-define (gen_server__cast_message_di_m1 (Method, Argument_1),
		?gen_server__cast_message_im1 (?gen_server__module, Method, Argument_1)).

-define (gen_server__cast_message_di_m2 (Method, Argument_1, Argument_2),
		?gen_server__cast_message_im2 (?gen_server__module, Method, Argument_1, Argument_2)).

-define (gen_server__cast_message_di_m3 (Method, Argument_1, Argument_2, Argument_3),
		?gen_server__cast_message_im3 (?gen_server__module, Method, Argument_1, Argument_2, Argument_3)).

-define (gen_server__cast_message_di_m4 (Method, Argument_1, Argument_2, Argument_3, Argument_4),
		?gen_server__cast_message_im4 (?gen_server__module, Method, Argument_1, Argument_2, Argument_3, Argument_4)).

-define (gen_server__cast_message_di_m5 (Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5),
		?gen_server__cast_message_im5 (?gen_server__module, Method, Argument_1, Argument_2, Argument_3, Argument_4, Argument_5)).


% ----------------------------------------------------------------------------
% ----- gen_server message interface aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_interface_aliases).
	
	-define (stop_message (A1), ?gen_server__stop_message (A1)).
	
	-define (call_message_im0 (A1, A2), ?gen_server__call_message_im0 (A1, A2)).
	-define (call_message_im1 (A1, A2, A3), ?gen_server__call_message_im1 (A1, A2, A3)).
	-define (call_message_im2 (A1, A2, A3, A4), ?gen_server__call_message_im2 (A1, A2, A3, A4)).
	-define (call_message_im3 (A1, A2, A3, A4, A5), ?gen_server__call_message_im3 (A1, A2, A3, A4, A5)).
	-define (call_message_im4 (A1, A2, A3, A4, A5, A6), ?gen_server__call_message_im4 (A1, A2, A3, A4, A5, A6)).
	-define (call_message_im5 (A1, A2, A3, A4, A5, A6, A7), ?gen_server__call_message_im5 (A1, A2, A3, A4, A5, A6, A7)).
	
	-define (call_message_di_m0 (A1), ?gen_server__call_message_di_m0 (A1)).
	-define (call_message_di_m1 (A1, A2), ?gen_server__call_message_di_m1 (A1, A2)).
	-define (call_message_di_m2 (A1, A2, A3), ?gen_server__call_message_di_m2 (A1, A2, A3)).
	-define (call_message_di_m3 (A1, A2, A3, A4), ?gen_server__call_message_di_m3 (A1, A2, A3, A4)).
	-define (call_message_di_m4 (A1, A2, A3, A4, A5), ?gen_server__call_message_di_m4 (A1, A2, A3, A4, A5)).
	-define (call_message_di_m5 (A1, A2, A3, A4, A5, A6), ?gen_server__call_message_di_m5 (A1, A2, A3, A4, A5, A6)).
	
	-define (cast_message_im0 (A1, A2), ?gen_server__cast_message_im0 (A1, A2)).
	-define (cast_message_im1 (A1, A2, A3), ?gen_server__cast_message_im1 (A1, A2, A3)).
	-define (cast_message_im2 (A1, A2, A3, A4), ?gen_server__cast_message_im2 (A1, A2, A3, A4)).
	-define (cast_message_im3 (A1, A2, A3, A4, A5), ?gen_server__cast_message_im3 (A1, A2, A3, A4, A5)).
	-define (cast_message_im4 (A1, A2, A3, A4, A5, A6), ?gen_server__cast_message_im4 (A1, A2, A3, A4, A5, A6)).
	-define (cast_message_im5 (A1, A2, A3, A4, A5, A6, A7), ?gen_server__cast_message_im5 (A1, A2, A3, A4, A5, A6, A7)).
	
	-define (cast_message_di_m0 (A1), ?gen_server__cast_message_di_m0 (A1)).
	-define (cast_message_di_m1 (A1, A2), ?gen_server__cast_message_di_m1 (A1, A2)).
	-define (cast_message_di_m2 (A1, A2, A3), ?gen_server__cast_message_di_m2 (A1, A2, A3)).
	-define (cast_message_di_m3 (A1, A2, A3, A4), ?gen_server__cast_message_di_m3 (A1, A2, A3, A4)).
	-define (cast_message_di_m4 (A1, A2, A3, A4, A5), ?gen_server__cast_message_di_m4 (A1, A2, A3, A4, A5)).
	-define (cast_message_di_m5 (A1, A2, A3, A4, A5, A6), ?gen_server__cast_message_di_m5 (A1, A2, A3, A4, A5, A6)).
	
-endif.


% ----- eof
