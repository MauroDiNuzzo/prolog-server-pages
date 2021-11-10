/*  ws_rpc.pl
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
 
 
 /*  
:- meta_predicate http_call(0).    
:- meta_predicate http_findall(?, 0, -).    
:- meta_predicate http_bagof(?, 0, -).    
:- meta_predicate http_setof(?, 0, -).    
*/
    
http_call(URL:Goal) :-
    (   is_absolute_url(URL)
    ->  true
    ;   throw(error(type_error(absolute_url, URL), context(http_call/1)))
    ),
    term_variables(Goal, Template),
    http_call(URL, findall(Template, Goal, Bag), Bag), !,
    select(Template, Bag, _).   % backtracking here
    
http_findall(Template, URL:Goal, Bag) :-
    (   is_absolute_url(URL)
    ->  true
    ;   throw(error(type_error(absolute_url, URL), context(http_findall/3)))
    ),
    http_call(URL, findall(Template, Goal, Bag), Bag).

http_bagof(Template, URL:Goal, Bag) :-
    (   is_absolute_url(URL)
    ->  true
    ;   throw(error(type_error(absolute_url, URL), context(http_bagof/3)))
    ),
    http_call(URL, bagof(Template, Goal, Bag), Bag), Bag \= [].

http_setof(Template, URL:Goal, Bag) :-
    (   is_absolute_url(URL)
    ->  true
    ;   throw(error(type_error(absolute_url, URL), context(http_setof/3)))
    ),
    http_call(URL, setof(Template, Goal, Bag), Bag), Bag \= [].



%% http_call(+URL, +Goal, -Reply)
%
% Perform Goal call on the specified URL, and
% unify the response with Reply (bag/list of solutions).
%
http_call(URL, Goal, Reply) :-  
    ws_user_agent(UserAgent),    
    http_log('webservices(sending_call, ~q).~n', [Goal]),    
    http_current_request(Request),
    memberchk(session(SessionID), Request),
    set_jsonrpc_request(InputJSON, 'prolog', Goal, SessionID),      
    http_post(URL, json(InputJSON), OutputJSON, [
        timeout(30), 
        user_agent(UserAgent)
    ]), 
    get_jsonrpc_response(OutputJSON, Term, Error, SessionID),
    http_log('webservices(received_response, ~q, ~q).~n', [Term, Error]),      
    (   Error = null
    -> Reply = Term
    ;   Reply = Error
    ). 


