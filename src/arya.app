%%	Copyright (C) 2012 Nazarenko Roman.
%%
%%	This file is part of Arya.
%%
%%	Arya is free software: you can redistribute it and/or modify
%%	it under the terms of the GNU General Public License as published by
%%	the Free Software Foundation, either version 3 of the License, or
%%	(at your option) any later version.
%%
%%	Arya is distributed in the hope that it will be useful,
%%	but WITHOUT ANY WARRANTY; without even the implied warranty of
%%	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%	GNU General Public License for more details.
%%
%%	You should have received a copy of the GNU General Public License
%%	along with Arya.  If not, see <http://www.gnu.org/licenses/>.

%%	Author: Nazarenko Roman <mailto: me@jtalk.me>
%%	License: <http://www.gnu.org/licenses/gpl.html>

%%% -----------------------------------------------------
%%% This is the Arya application .app file.
%%% -----------------------------------------------------

{application, arya,
	[
		{
			modules, 
			[
				arya,
				arya_main_sup,
				
				arya_server,
				arya_token_storage,
				
				arya_proc_sup,
				arya_processor,
				
				arya_dl_sup,
				arya_downloader,
				
				arya_common,
				
				assoc
			]
		},
		{ 
			mod, 
			{ arya, {2, 3600}} 
		},
		{ 
			env, 
			[
				{ log, "/var/logs/arya" },
				{ domain, [ <<"me">>, <<"jtalk">>, <<"d">>]},
				{ max_token, 999999 },
				{ dns_port, 53},
				{ sleep_time, 200},
				{ timeout, 2500},
				{ logfile, { open, "./log.txt"}},
				{ is_tty, true},
				{ dns_params, [ { active, true}, binary]},
				{ loglevel, 2}
			]
		}
	]
}.