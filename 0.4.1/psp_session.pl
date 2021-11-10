/*  http_psp_session.pl
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


:- redefine_system_predicate(http_session:open_session(_, _)).
:- redefine_system_predicate(http_session:valid_session_id(_, _)).
:- redefine_system_predicate(http_session:http_set_session(_)).
:- redefine_system_predicate(http_session:get_last_used(_, _)).
:- redefine_system_predicate(http_session:set_last_used(_, _)).
:- redefine_system_predicate(http_session:http_session_asserta(_)).
:- redefine_system_predicate(http_session:http_session_assert(_)).
:- redefine_system_predicate(http_session:http_session_retract(_)).
:- redefine_system_predicate(http_session:http_session_retractall(_)).

http_session:open_session(SessionID, Peer) :-
    get_time(Now),
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    setup_call_cleanup(open(File, write, Stream),
      (   format(Stream, ':- assert(http_session:current_session(~q, ~q)).~n', [SessionID, Peer]),
          format(Stream, ':- assert(http_session:last_used(~q, ~q)).~n', [SessionID, Now])
      ),
      close(Stream)
    ),
    broadcast(http_session(begin(SessionID, Peer))),
    load_files(File, [silent(true)]).

http_session:valid_session_id(SessionID, Peer) :-
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    exists_file(File),
    load_files(File, [silent(true)]),
    http_session:current_session(SessionID, SessionPeer),
    get_time(Now),
    (   http_session:session_setting(SessionID, timeout(Timeout)),
        Timeout > 0
    ->  http_session:get_last_used(SessionID, Last),
        Idle is Now - Last,
        (	Idle =< Timeout
        ->  true
        ;   http_session:http_close_session(SessionID), 
      fail
        )
    ;   Peer \== SessionPeer
    ->  http_session:http_close_session(SessionID), 
        fail
    ;   true
    ),
    http_session:set_last_used(SessionID, Now).

http_session:http_set_session(Setting) :-
    http_session:http_session_id(SessionID),
    functor(Setting, Name, Arity),
    (   http_session:local_option(Name, _, _)
    ->  true
    ;   permission_error(set, http_session, Setting)
    ),
    arg(1, Setting, Value),
    (   http_session:session_option(Name, Type)
    ->  must_be(Type, Value)
    ;   domain_error(http_session_option, Setting)
    ),
    functor(Free, Name, Arity),
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    setup_call_cleanup(open(File, write, Stream),
      (
          format(Stream, ':- retractall(http_session:session_data(~q, ''$setting''(~q))).~n', [SessionID, Free]),
          format(Stream, ':- assert(http_session:session_data(~q, ''$setting''(~q))).~n', [SessionID, Setting])          
      ),
      close(Stream)
    ).
    
http_session:get_last_used(SessionID, Last) :-
    http_session:last_used(SessionID, Last), !.
http_session:get_last_used(SessionID, Last) :-    
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),    
    exists_file(File),
    load_files(File, [silent(true)]),
    http_session:last_used(SessionID, Last).

http_session:set_last_used(SessionID, Now) :-
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    setup_call_cleanup(open(File, append, Stream),
      (   format(Stream, ':- retractall(http_session:last_used(_, _)).~n', []),
          format(Stream, ':- assert(http_session:last_used(~q, ~q)).~n', [SessionID, Now])
      ),
      close(Stream)
    ). 
 
http_session:http_session_asserta(Data) :-
    http_session:http_session_id(SessionID), 
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    setup_call_cleanup(open(File, append, Stream),
      format(Stream, ':- asserta(http_session:session_data(~q, ~q)).~n', [SessionID, Data]),
      close(Stream)
    ).
    
http_session:http_session_assert(Data) :-
    http_session:http_session_id(SessionID), 
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    setup_call_cleanup(open(File, append, Stream),
      format(Stream, ':- assert(http_session:session_data(~q, ~q)).~n', [SessionID, Data]),
      close(Stream)
    ).    
    
http_session:http_session_retract(Data) :-
    http_session:http_session_id(SessionID), 
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    setup_call_cleanup(open(File, append, Stream),
      format(Stream, ':- retract(http_session:session_data(~q, ~q)).~n', [SessionID, Data]),
      close(Stream)
    ).    
    
http_session:http_session_retractall(Data) :-
    http_session:http_session_id(SessionID), 
    absolute_file_name(http_session_data(SessionID), File, [extensions([pl])]),
    setup_call_cleanup(open(File, append, Stream),
      format(Stream, ':- retractall(http_session:session_data(~q, ~q)).~n', [SessionID, Data]),
      close(Stream)
    ).        

:- compile_predicates([http_session:open_session/2]).
:- compile_predicates([http_session:valid_session_id/2]).
:- compile_predicates([http_session:http_set_session/1]).
:- compile_predicates([http_session:get_last_used/2]).    
:- compile_predicates([http_session:set_last_used/2]).
:- compile_predicates([http_session:http_session_asserta/1]).
:- compile_predicates([http_session:http_session_assert/1]).
:- compile_predicates([http_session:http_session_retract/1]).
:- compile_predicates([http_session:http_session_retractall/1]). 


    