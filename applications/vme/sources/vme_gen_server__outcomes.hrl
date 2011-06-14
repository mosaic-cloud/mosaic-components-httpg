
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
% ----- gen_server outcomes
% ----------------------------------------------------------------------------


% ----- generic

-define (gen_server__ok,
		ok).

-define (gen_server__ok_1 (Value),
		{ok, Value}).

-define (gen_server__error (Error),
		{error, Error}).

-define (gen_server__invalid_argument (Argument),
		?gen_server__error ({invalid_argument, Argument})).

-define (gen_server__invalid_message (Message, Sender),
		?gen_server__error ({invalid_message, Message, Sender})).

-define (gen_server__unknown_message (Message, Sender),
		?gen_server__error ({unknown_message, Message, Sender})).


% ----- init

-define (gen_server__init_ok (State),
		{ok, State}).

-define (gen_server__init_stop (StopReason),
		{stop, StopReason}).

-define (gen_server__init_stop_with_error (Error),
		?gen_server__init_stop (?gen_server__error (Error))).

-define (gen_server__init_stop_with_invalid_argument (Argument),
		?gen_server__init_stop (?gen_server__invalid_argument (Argument))).


% ----- call reply

-define (gen_server__call_reply (Reply, NewState),
		{reply, Reply, NewState}).

-define (gen_server__call_reply_ok (NewState),
		?gen_server__call_reply (?gen_server__ok, NewState)).

-define (gen_server__call_reply_ok_1 (Value, NewState),
		?gen_server__call_reply (?gen_server__ok_1 (Value), NewState)).

-define (gen_server__call_reply_error (Error, NewState),
		?gen_server__call_reply (?gen_server__error (Error), NewState)).

-define (gen_server__call_reply_invalid_message (Message, Sender, NewState),
		?gen_server__call_reply (?gen_server__invalid_message (Message, Sender), NewState)).

-define (gen_server__call_reply_unknown_message (Message, Sender, NewState),
		?gen_server__call_reply (?gen_server__unknown_message (Message, Sender), NewState)).


% ----- call stop_with_reply

-define (gen_server__call_stop_with_reply (Reply, StopReason, NewState),
		{stop, StopReason, Reply, NewState}).

-define (gen_server__call_stop_with_reply_y (StopReason, NewState),
		{stop, StopReason, StopReason, NewState}).

-define (gen_server__call_stop_with_reply_ok (NewState),
		?gen_server__call_stop_with_reply (?gen_server__ok, normal, NewState)).

-define (gen_server__call_stop_with_reply_ok_1 (Value, StopReason, NewState),
		?gen_server__call_stop_with_reply (?gen_server__ok_1 (Value), normal, NewState)).

-define (gen_server__call_stop_with_reply_error (Error, NewState),
		?gen_server__call_stop_with_reply_y (?gen_server__error (Error), NewState)).

-define (gen_server__call_stop_with_reply_invalid_message (Message, Sender, NewState),
		?gen_server__call_stop_with_reply_y (?gen_server__invalid_message (Message, Sender), NewState)).

-define (gen_server__call_stop_with_reply_unknown_message (Message, Sender, NewState),
		?gen_server__call_stop_with_reply_y (?gen_server__unknown_message (Message, Sender), NewState)).


% ----- call noreply

-define (gen_server__call_noreply (NewState),
		{noreply, NewState}).


% ----- call stop_with_noreply

-define (gen_server__call_stop_with_noreply (StopReason, NewState),
		{stop, StopReason, NewState}).

-define (gen_server__call_stop_with_noreply_ok (NewState),
		?gen_server__call_stop_with_noreply (normal, NewState)).

-define (gen_server__call_stop_with_noreply_error (Error, NewState),
		?gen_server__call_stop_with_noreply (?gen_server__error (Error), NewState)).

-define (gen_server__call_stop_with_noreply_invalid_message (Message, NewState),
		?gen_server__call_stop_with_noreply (?gen_server__invalid_message (Message, undefined), NewState)).

-define (gen_server__call_stop_with_noreply_unknown_message (Message, NewState),
		?gen_server__call_stop_with_noreply (?gen_server__unknown_message (Message, undefined), NewState)).


% ----- cast noreply

-define (gen_server__cast_noreply (NewState),
		{noreply, NewState}).


% ----- cast stop_with_noreply

-define (gen_server__cast_stop_with_noreply (StopReason, NewState),
		{stop, StopReason, NewState}).

-define (gen_server__cast_stop_with_noreply_ok (NewState),
		?gen_server__cast_stop_with_noreply (normal, NewState)).

-define (gen_server__cast_stop_with_noreply_error (Error, NewState),
		?gen_server__cast_stop_with_noreply (?gen_server__error (Error), NewState)).

-define (gen_server__cast_stop_with_noreply_invalid_message (Message, NewState),
		?gen_server__cast_stop_with_noreply (?gen_server__invalid_message (Message, undefined), NewState)).

-define (gen_server__cast_stop_with_noreply_unknown_message (Message, NewState),
		?gen_server__cast_stop_with_noreply (?gen_server__unknown_message (Message, undefined), NewState)).


% ----- info noreply

-define (gen_server__info_noreply (NewState),
		{noreply, NewState}).


% ----- info stop_with_noreply

-define (gen_server__info_stop_with_noreply (StopReason, NewState),
		{stop, StopReason, NewState}).

-define (gen_server__info_stop_with_noreply_ok (NewState),
		?gen_server__info_stop_with_noreply (normal, NewState)).

-define (gen_server__info_stop_with_noreply_error (Error, NewState),
		?gen_server__info_stop_with_noreply (?gen_server__error (Error), NewState)).

-define (gen_server__info_stop_with_noreply_invalid_message (Message, NewState),
		?gen_server__info_stop_with_noreply (?gen_server__invalid_message (Message, undefined), NewState)).

-define (gen_server__info_stop_with_noreply_unknown_message (Message, NewState),
		?gen_server__info_stop_with_noreply (?gen_server__unknown_message (Message, undefined), NewState)).


% ----------------------------------------------------------------------------
% ----- gen_server outcome interface aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_interface_aliases).
	
	-define (ok, ?gen_server__ok).
	-define (ok_1 (A1), ?gen_server__ok_1 (A1)).
	-define (error (A1), ?gen_server__error (A1)).
	-define (invalid_argument (A1), ?gen_server__invalid_argument (A1)).
	-define (invalid_message (A1, A2), ?gen_server__invalid_message (A1, A2)).
	-define (unknown_message (A1, A2), ?gen_server__unknown_message (A1, A2)).
	
-endif.


% ----------------------------------------------------------------------------
% ----- gen_server outcome internal aliases
% ----------------------------------------------------------------------------


-ifdef (gen_server__define_internal_aliases).
	
	-define (init_ok (A1), ?gen_server__init_ok (A1)).
	-define (init_stop (A1), ?gen_server__init_stop (A1)).
	-define (init_stop_with_error (A1), ?gen_server__init_stop_with_error (A1)).
	-define (init_stop_with_invalid_argument (A1), ?gen_server__init_stop_with_invalid_argument (A1)).
	
	-define (call_reply (A1, A2), ?gen_server__call_reply (A1, A2)).
	-define (call_reply_ok (A1), ?gen_server__call_reply_ok (A1)).
	-define (call_reply_ok_1 (A1, A2), ?gen_server__call_reply_ok_1 (A1, A2)).
	-define (call_reply_error (A1, A2), ?gen_server__call_reply_error (A1, A2)).
	-define (call_reply_invalid_message (A1, A2, A3), ?gen_server__call_reply_invalid_message (A1, A2, A3)).
	-define (call_reply_unknown_message (A1, A2, A3), ?gen_server__call_reply_unknown_message (A1, A2, A3)).
	
	-define (call_stop_with_reply (A1, A2, A3), ?gen_server__call_stop_with_reply (A1, A2, A3)).
	-define (call_stop_with_reply_ok (A1), ?gen_server__call_stop_with_reply_ok (A1)).
	-define (call_stop_with_reply_ok_1 (A1, A2), ?gen_server__call_stop_with_reply_ok_1 (A1, A2)).
	-define (call_stop_with_reply_error (A1, A2), ?gen_server__call_stop_with_reply_error (A1, A2)).
	-define (call_stop_with_reply_invalid_message (A1, A2, A3), ?gen_server__call_stop_with_reply_invalid_message (A1, A2, A3)).
	-define (call_stop_with_reply_unknown_message (A1, A2, A3), ?gen_server__call_stop_with_reply_unknown_message (A1, A2, A3)).
	
	-define (call_noreply (A1), ?gen_server__call_noreply (A1)).
	
	-define (call_stop_with_noreply (A1, A2), ?gen_server__call_stop_with_noreply (A1, A2)).
	-define (call_stop_with_noreply_ok (A1), ?gen_server__call_stop_with_noreply_ok (A1)).
	-define (call_stop_with_noreply_error (A1, A2), ?gen_server__call_stop_with_noreply_error (A1, A2)).
	-define (call_stop_with_noreply_invalid_message (A1, A2, A3), ?gen_server__call_stop_with_noreply_invalid_message (A1, A2, A3)).
	-define (call_stop_with_noreply_unknown_message (A1, A2, A3), ?gen_server__call_stop_with_noreply_unknown_message (A1, A2, A3)).
	
	-define (cast_noreply (A1), ?gen_server__cast_noreply (A1)).
	
	-define (cast_stop_with_noreply (A1, A2), ?gen_server__cast_stop_with_noreply (A1, A2)).
	-define (cast_stop_with_noreply_ok (A1), ?gen_server__cast_stop_with_noreply_ok (A1)).
	-define (cast_stop_with_noreply_error (A1, A2), ?gen_server__cast_stop_with_noreply_error (A1, A2)).
	-define (cast_stop_with_noreply_invalid_message (A1, A2), ?gen_server__cast_stop_with_noreply_invalid_message (A1, A2)).
	-define (cast_stop_with_noreply_unknown_message (A1, A2), ?gen_server__cast_stop_with_noreply_unknown_message (A1, A2)).
	
	-define (info_noreply (A1), ?gen_server__info_noreply (A1)).
	
	-define (info_stop_with_noreply (A1, A2), ?gen_server__info_stop_with_noreply (A1, A2)).
	-define (info_stop_with_noreply_ok (A1), ?gen_server__info_stop_with_noreply_ok (A1)).
	-define (info_stop_with_noreply_error (A1, A2), ?gen_server__info_stop_with_noreply_error (A1, A2)).
	-define (info_stop_with_noreply_invalid_message (A1, A2), ?gen_server__info_stop_with_noreply_invalid_message (A1, A2)).
	-define (info_stop_with_noreply_unknown_message (A1, A2), ?gen_server__info_stop_with_noreply_unknown_message (A1, A2)).
	
-endif.


% ----- eof
