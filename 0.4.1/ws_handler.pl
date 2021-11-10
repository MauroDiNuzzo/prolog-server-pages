/*  ws_handler.pl
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
 
 
:- dynamic user:cgi_format_hook/2.
:- multifile user:cgi_format_hook/2.     


%% ws_handler(+Request)
%
% Handle a RPC request. This predicate is called by the PSP handler and therefore it is not returned by http_current_handler/[2,3].  
               
ws_handler(Request) :-
    ws_user_agent(UserAgent),
    memberchk(user_agent(UserAgent), Request),
    memberchk(method(post), Request), 
    memberchk(content_type('application/json'), Request), !,    
    catch(http_read_json(Request, InputJSON, [json_object(term)]), _, fail),    
    get_jsonrpc_request(InputJSON, 'prolog', Term, ID), 
    http_log('webservices(received_call, ~q).~n', [Term]),      
    memberchk(path_translated(File), Request),
    assert(user:cgi_format_hook(_, _)),   % load file with suppressed output
    user:load_files(File, [silent(true)]),
    retract(user:cgi_format_hook(_, _)),
    ws_call(Term, Reply, Error),
    http_log('webservices(sending_response, ~q, ~q).~n', [Reply, Error]),    
    set_jsonrpc_response(OutputJSON, Reply, Error, ID),
    reply_json(OutputJSON, [json_object(term), width(0), serialize_unknown(true)]).
      
ws_user_agent('SWI-Prolog (http://www.swi-prolog.org)').        % I am using SWI default, as http_post/4 seems not to overrule user_agent(atom) option
       
ws_call(Term, Reply, Error) :-
    Term =.. [Functor, _, _, Reply],
    memberchk(Functor, [findall, bagof, setof]),
    catch((user:Term, Error = null), Error, Reply = null), !.
ws_call(Term, null, Error) :-    
    Error = error(type_error(rpc_goal, Term), _).
 
 
 
set_jsonrpc_request(Request, Method, Term, ID) :-
    var(Request),
    term_to_atom(Term, Atom),  % work as ? ?
    Request = json([
        jsonrpc = '2.0',
        method = Method,
        params = [Atom],
        id = ID
    ]).
    
get_jsonrpc_request(json(Request), Method, Term, ID) :-
    ground(Request),
    memberchk(method = Method, Request),
    memberchk(params = [Atom], Request),
    memberchk(id = ID, Request),
    term_to_atom(Term, Atom).  % work as ? ?

set_jsonrpc_response(Response, Term, null, ID) :- !,
    var(Response),
    term_to_atom(Term, Atom),
    Response = json([
        jsonrpc = '2.0',
        result = [Atom],
        error = null,
        id = ID
    ]).
set_jsonrpc_response(Response, _, Term, ID) :- 
    var(Response),
    term_to_atom(Term, Atom),
    Response = json([
        jsonrpc = '2.0',
        result = null,
        error = json([code = -32000, message = Atom]),
        id = ID
    ]).    

get_jsonrpc_response(json(Response), null, Term, ID) :- 
    ground(Response),
    memberchk(result = null, Response), !,
    memberchk(error = json(T), Response),
    memberchk(code = -32000, T),
    memberchk(message = Atom, T),
    memberchk(id = ID, Response),    
    term_to_atom(Term, Atom).
get_jsonrpc_response(json(Response), Term, null, ID) :- 
    ground(Response),
    memberchk(error = null, Response), 
    memberchk(result = [Atom], Response),
    memberchk(id = ID, Response),
    term_to_atom(Term, Atom).
    


    
 