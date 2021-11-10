/*  cgi_httpd.pl
 *
 *      Part of Prolog Server Pages (http://www.prologwebservices.com/)
 *      Copyright (C) 2013-2015 by Mauro DiNuzzo. All Rights Reserved.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *   
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston,
 *  MA 02110-1301  USA 
 */ 
 
 % This file is based on library(http/thread_httpd)

:- module(cgi_httpd, 
	  [ http_current_server/2,	% ?:Goal, ?Port
	    http_server_property/2,	% ?Port, ?Property
	    http_server/2,		% :Goal, +Options
	    http_stop_server/2,		% +Port, +Options
	    http_spawn/2,		% :Goal, +Options
	    http_close_connection/1	  % +Request
	  ]).
:- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(socket)).
:- use_module(library(thread_pool)).
:- use_module(library(gensym)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_path)).
:- use_module(library(http/http_log)).


:- predicate_options(http_server/2, 2,
		     [ port(any),
		       timeout(number),
		       keep_alive_timeout(number),
		       ssl(list(any)),	% if http/http_ssl_plugin is loaded
		       pass_to(system:thread_create/3, 3)
		     ]).
:- predicate_options(http_spawn/2, 2,
		     [ pool(atom),
		       pass_to(system:thread_create/3, 3),
		       pass_to(thread_pool:thread_create_in_pool/4, 4)
		     ]).


:- meta_predicate
    http_server(1, :),
    http_current_server(1, ?),
    http_spawn(0, +).

:- dynamic
    current_server/6,	% Port, Goal, Thread, Thread, Scheme, StartTime
    cgi_streams/2.    % In, Out


http_server(Goal, _:Options) :-
    select_option(port(Port), Options, Options1), !,
    getenv('SERVER_PORT', Port),    % retrieve port from CGI environment variable	
    get_time(StartTime),
    scheme(Scheme, Options1),
    thread_self(Thread), 
    assert(current_server(Port, Goal, Thread, Thread, Scheme, StartTime)),
    current_input(In),
    current_output(Out),
    assert(cgi_streams(In, Out)),
    http_process(Goal, In, Out, Options1),    
    halt.
http_server(_Goal, _Options) :-
    existence_error(option, port).


     

scheme(Scheme, Options) :-
	option(scheme(Scheme), Options), !.
scheme(Scheme, Options) :-
	option(ssl(_), Options), !,
	Scheme = https.
scheme(http, _).


http_current_server(Goal, Port) :-
	current_server(Port, Goal, _, _, _, _).


http_server_property(Port, Property) :-
	server_property(Property, Port).

server_property(goal(Goal), Port) :-
	current_server(Port, Goal, _, _, _, _).
server_property(scheme(Scheme), Port) :-
	current_server(Port, _, _, _, Scheme, _).
server_property(start_time(Time), Port) :-
	current_server(Port, _, _, _, _, Time).


http_stop_server(_Port, _Options) :-
    halt.


http_process(Goal, In, Out, Options) :-
	debug(http(server), 'Running server goal ~p on ~p -> ~p',
	      [Goal, In, Out]),
http_log('%\n', [Goal]),   	      
	http_wrapper(Goal, In, Out, Connection,
		     [ request(Request)
		     | Options
		     ]), 
	next(Connection, Request).


next(switch_protocol(SwitchGoal, _SwitchOptions), _Request) :- !,
  current_input(In),
  current_output(Out),
  call(SwitchGoal, In, Out). % removed catch/3
next(spawned(ThreadId), _) :- !,
	debug(http(spawn), 'Handler spawned to thread ~w', [ThreadId]).
next(_, Request) :-
	http_close_connection(Request).


http_close_connection(Request) :-
	memberchk(pool(client(_Queue, _Goal, In, Out)), Request),
	catch(close(In, [force(true)]), _, true),
	catch(close(Out, [force(true)]), _, true).


http_spawn(Goal, Options) :-
	select_option(pool(Pool), Options, ThreadOptions), !,
	current_output(CGI),
	catch(thread_create_in_pool(Pool,
				    wrap_spawned(CGI, Goal), Id,
				    [ detached(true)
				    | ThreadOptions
				    ]),
	      Error,
	      true),
	(   var(Error)
	->  http_spawned(Id)
	;   Error = error(resource_error(threads_in_pool(_)), _)
	->  throw(http_reply(busy))
	;   Error = error(existence_error(thread_pool, Pool), _),
	    create_pool(Pool)
	->  http_spawn(Goal, Options)
	;   throw(Error)
	).
http_spawn(Goal, Options) :-
  http_current_request(Request),
  memberchk(pool(client(_Queue, _Goal, In, Out)), Request),
	thread_create(wrap_spawned(In, Out, Goal), Id, 
		      [ detached(false)   % true does not allow caaling thread_join/2
		      | Options
		      ]),
	http_spawned(Id),
  thread_join(Id, Status),
  http_log('server(spawned(~w), status(~q)).\n', [Id, Status]).

wrap_spawned(_In, _Out, Goal) :-    % In, Out not necessary if the thread is run with detached(false)
  %set_input(In),
	%set_output(Out),
	http_wrap_spawned(Goal, Request, Connection),
	next(Connection, Request).

create_pool(Pool) :-
	E = error(permission_error(create, thread_pool, Pool), _),
	catch(http:create_pool(Pool), E, true).
create_pool(Pool) :-
	print_message(informational, httpd(created_pool(Pool))),
	thread_pool_create(Pool, 10, []).



%	thread_httpd:message_level(+Exception, -Level)
%
%	Determine the message stream used for  exceptions that may occur
%	during server_loop/5. Being multifile, clauses   can be added by
%	the   application   to   refine   error   handling.   See   also
%	message_hook/3 for further programming error handling.

:- multifile
	message_level/2.

message_level(error(io_error(read, _), _),	silent).
message_level(error(timeout_error(read, _), _),	informational).
message_level(keep_alive_timeout,		silent).

current_message_level(Term, Level) :-
	(   message_level(Term, Level)
	->  true
	;   Level = error
	).



		 /*******************************
		 *	      MESSAGES		*
		 *******************************/

:- multifile
	prolog:message/3.

prolog:message(httpd_started_server(Port)) -->
	[ 'Started server at '-[] ],
	http_root(Port).
prolog:message(httpd_stopped_worker(Self, Status)) -->
	[ 'Stopped worker ~p: ~p'-[Self, Status] ].
prolog:message(httpd_restarted_worker(Self)) -->
	[ 'Replaced aborted worker ~p'-[Self] ].
prolog:message(httpd(created_pool(Pool))) -->
	[ 'Created thread-pool ~p of size 10'-[Pool], nl,
	  'Create this pool at startup-time or define the hook ', nl,
	  'http:create_pool/1 to avoid this message and create a ', nl,
	  'pool that fits the usage-profile.'
	].

http_root(Host:Port) --> !,
	http_scheme(Host:Port),
	{ http_absolute_location(root(.), URI, []) },
	[ '~w:~w~w'-[Host, Port, URI] ].
http_root(Port) -->
	http_scheme(Port),
	{ http_absolute_location(root(.), URI, []) },
	[ 'localhost:~w~w'-[Port, URI] ].

http_scheme(Port) -->
	{ http_server_property(Port, scheme(Scheme)) },
	[ '~w://'-[Scheme] ].

