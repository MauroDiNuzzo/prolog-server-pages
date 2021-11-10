/*  http_cgi.pl
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


:- module(http_cgi, [
        cgi_variable/1,
        cgi_format/2
    ]).
    
:- use_module(library(http/http_stream)).       
:- use_module(library(http/http_wrapper)).

:- use_module(library(url)). 

    
%% cgi_variable(?Term)
%
% True if Term is a valid element of HTTP request, which is currently defined
% as a CGI environment variable. This predicate is used to generate a HTTP request
% suitable for http_wrapper/5 by redefining the predicate http_header:http_read_request/2.
%
% @todo Implements the set_cookie field (necessary for session handling).

cgi_variable(document_root(Root)) :-
    getenv('DOCUMENT_ROOT', Root).
cgi_variable(host(Host)) :-
    getenv('HTTP_HOST', Host).
cgi_variable(remote_host(Host)) :-
    getenv('REMOTE_HOST', Host).    
cgi_variable(remote_addr(Addr)) :-
    getenv('REMOTE_ADDR', Addr).        
cgi_variable(remote_user(User)) :-
    getenv('REMOTE_USER', User).        
cgi_variable(method(Method)) :-
    getenv('REQUEST_METHOD', Value),
    downcase_atom(Value, Method).    
cgi_variable(script_name(Name)) :-
    getenv('SCRIPT_NAME', Name).    
cgi_variable(script_filename(FileName)) :-
    getenv('SCRIPT_FILENAME', Value),
    prolog_to_os_filename(FileName, Value).      
cgi_variable(path(Path)) :-
    getenv('PATH_INFO', Path).
cgi_variable(path_translated(Path)) :-
    getenv('PATH_TRANSLATED', Value),
    prolog_to_os_filename(Path, Value).    
cgi_variable(peer(ip(A, B, C, D))) :-
    getenv('REMOTE_ADDR', Value),
    atomic_list_concat(Atomic, '.', Value),
    maplist(atom_number, Atomic, [A, B, C, D]).
cgi_variable(server_name(Name)) :-
    getenv('SERVER_NAME', Name).    
cgi_variable(server_protocol(Protocol)) :-
    getenv('SERVER_PROTOCOL',Value),
    downcase_atom(Value, Protocol).        
cgi_variable(server_admin(Admin)) :-
    getenv('SERVER_ADMIN', Admin).
cgi_variable(port(Port)) :-
    getenv('SERVER_PORT', Atom),
    atom_number(Atom, Port).
cgi_variable(request_uri(URI)) :-
    getenv('REQUEST_URI', URI).    
cgi_variable(search(Search)) :-
    getenv('QUERY_STRING', Value),
    parse_url_search(Value, Search).
cgi_variable(http_version(Major-Minor)) :-
    getenv('SERVER_PROTOCOL', Value),
    atomic_list_concat(['HTTP', Version], '/', Value),
    atomic_list_concat(Atomic, '.', Version),
    maplist(atom_number, Atomic, [Major, Minor]).
cgi_variable(authorization(Authorization)) :-
    getenv('AUTH_TYPE', Value),
    downcase_atom(Value, Authorization).        
cgi_variable(content_type(Type)) :-
    getenv('CONTENT_TYPE', Type).
cgi_variable(content_length(Length)) :-
    getenv('CONTENT_LENGTH', Atom),
    atom_number(Atom, Length),
    Length > 0.
cgi_variable(connection(Connection)) :-
    getenv('HTTP_CONNECTION', Value),
    downcase_atom(Value, Connection).      
cgi_variable(referer(Referer)) :-
    getenv('HTTP_REFERER', Referer).
cgi_variable(accept(Accept)) :-
    getenv('HTTP_ACCEPT', Accept).    
cgi_variable(accept_charset(Accept)) :-
    getenv('HTTP_ACCEPT_CHARSET', Accept).        
cgi_variable(accept_encoding(Accept)) :-
    getenv('HTTP_ENCODING', Accept).    
cgi_variable(accept_language(Accept)) :-
    getenv('HTTP_ACCEPT_LANGUAGE', Accept).        
cgi_variable(user_agent(UserAgent)) :-
    getenv('HTTP_USER_AGENT', UserAgent).
cgi_variable(cookie(Cookie)) :-
    getenv('HTTP_COOKIE', Value),
    atomic_list_concat(List, ';', Value),
    maplist(cookie_to_pair, List, Cookie). 
%cgi_variable(set_cookie(Cookie)) :-    % ???????????????????????????
cgi_variable(origin(Origin)) :-
    getenv('HTTP_ORIGIN', Origin).
cgi_variable(gateway_interface(Interface)) :-
    getenv('GATEWAY_INTERFACE', Interface).
cgi_variable(server_software(Software)) :-
    getenv('SERVER_SOFTWARE', Software).
cgi_variable(unique_id(ID)) :-
    getenv('UNIQUE_ID', ID).
cgi_variable(sec_websocket_version(Version)) :-
    getenv('HTTP_SEC_WEBSOCKET_VERSION', Value),
    atom_number(Value, Version).    
cgi_variable(sec_websocket_protocol(Protocol)) :-
    getenv('HTTP_SEC_WEBSOCKET_PROTOCOL', Value),
    downcase_atom(Value, Protocol).    
cgi_variable(sec_websocket_key(ClientKey)) :-
    getenv('HTTP_SEC_WEBSOCKET_KEY', ClientKey).
cgi_variable(upgrade(Upgrade)) :-
    getenv('HTTP_UPGRADE', Value),
    downcase_atom(Value, Upgrade).    
    
cookie_to_pair(Atom, Name=Value) :-
    atomic_list_concat([Name, Value], '=', Atom).

    
:- dynamic current_dtd/1.     % set in "psp_load_files.pl"

%% cgi_format(+Fmt, +Args)
%
% This predicate behaves like format/2 but it checks whether HTTP headers have already been sent.
% Hookable via user:cgi_format_hook/2 (normally not defined).
%
cgi_format(_, _) :-
    http_current_request(Request),
    memberchk(connection(websocket), Request), !.
cgi_format(Fmt, Args) :- 
    user:cgi_format_hook(Fmt, Args), !.
cgi_format(Fmt, Args) :-    
    cgi_property(current_output, state(header)), !,
    write('Content-type: text/html; charset=utf-8\r\n\r\n'),   
    (current_dtd(DTD) -> (dtd_property(DTD, doctype(DocType)), format('<!DOCTYPE ~w>\r\n', [DocType])) ; true),
    format(Fmt, Args).
cgi_format(Fmt, Args) :-        
    cgi_property(current_output, state(data)), !,
    format(Fmt, Args).

:- dynamic user:cgi_format_hook/2.
:- multifile user:cgi_format_hook/2.    

