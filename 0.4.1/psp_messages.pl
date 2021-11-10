/*  psp_messages.pl
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
 
 
:- create_prolog_flag(http_reporting, [], [type(term)]).  

:- dynamic message/3.
:- dynamic current_request/1.

:- dynamic user:message_hook/3.
:- multifile user:message_hook/3.

user:message_hook(Term, _, _) :-
    Term =.. [http_reply|_], !,
    throw(Term).
user:message_hook(Term, Kind, Lines) :- 
    http_current_request(_),  % yes we are online
    http_log('message(~w, ~q).~n', [Kind, Term]),
    assert(message(Term, Kind, Lines)).
    
http_cleanup(Request) :-   
    memberchk(path_translated(Path), Request),
    message(Term, Kind, _Lines), 
    current_prolog_flag(http_reporting, Messages),
    memberchk(Kind, Messages),
    translate_term(Path, Term, Translated),
    assert(current_request(Request)),    
    throw(http_reply(server_error(Translated))), !.
http_cleanup(_).

translate_term(Path, Term, Translated) :-
    Term =.. [Functor, Arg, context(_, A)], !,
    Translated =.. [Functor, Arg, context(Path, A)].
translate_term(Path, Term, Translated) :-
    Term =.. [Functor, Arg, file(_, A, B, C)], !,
    Translated =.. [Functor, Arg, file(Path, A, B, C)].    
translate_term(_, Term, Term).


:- dynamic http:http_address//0.
:- multifile http:http_address//0.

http:http_address -->
	{ current_request(Request),
    memberchk(server_protocol(ServerProtocol), Request),
    atomic_list_concat([Protocol, _], '/', ServerProtocol),
    memberchk(host(Host), Request),    
    memberchk(port(Port), Request),
    memberchk(path(Path), Request),    
    parse_url(URL, [protocol(Protocol), host(Host), port(Port), path(Path)]),
    gethostname(HostName) 
    }, 
  html(p([b(['Requested URL: ']), URL])),
	html(address([ a(href('http://www.prologwebservices.com/'), 'Prolog Web Services'),
		       ' httpd at ', HostName
		     ])).


  