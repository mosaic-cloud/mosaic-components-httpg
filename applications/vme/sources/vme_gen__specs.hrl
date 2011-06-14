
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
% ----- gen specs
% ----------------------------------------------------------------------------


% ----- start names

-type (gen__start_local_name () ::
		{local, atom ()}).

-type (gen__start_global_name () ::
		{global, atom ()}).

-type (gen__start_name () ::
		gen__start_local_name () |
		gen__start_global_name ()).


% ----- send names

-type (gen__send_local_name () ::
		process ()).

-type (gen__send_remote_name () ::
		{atom (), atom ()}).

-type (gen__send_global_name () ::
		{global, atom ()}).

-type (gen__send_name () ::
		gen__send_local_name () |
		gen__send_remote_name () |
		gen__send_global_name ()).


% ----- start outcomes

-type (gen__start_outcome () ::
		{ok, pid ()} |
		{error, any ()} |
		ignore).


% ----- stop outcomes

-type (gen__stop_outcome () ::
		ok |
		{error, any ()}).


% ----- eof
