/*  http_psp_header.pl
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


:- redefine_system_predicate(http_header:http_read_request(_, _)).

http_header:http_read_request(Stream, Request) :-
    thread_self(Thread),
    cgi_httpd:cgi_streams(In, Out),
    findall(Term, cgi_variable(Term), Request0),
    append([input(Stream), pool(client(Thread, http_dispatch, In, Out))], Request0, Request1),
    sort(Request1, Request).

:- compile_predicates([http_header:http_read_request/2]). 

