/*  http_psp.pl
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

:- module(http_psp, [
        psp_handler/1,
        http_call/1,
        http_findall/3,
        http_bagof/3,
        http_setof/3        
    ]).

:- set_prolog_flag(double_quotes, codes).
 
:- use_module(library(http/http_client)).    
:- use_module(library(http/http_dispatch)).  
:- use_module(library(http/http_wrapper)).    
:- use_module(library(http/http_log)).   
:- use_module(library(http/http_header)).
:- use_module(library(http/http_open)).
:- use_module(library(http/http_stream)).
:- use_module(library(http/http_path)).
    
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).
:- use_module(library(http/http_json)).    
    
:- use_module(library(sgml)).    
:- use_module(library(url)).    
:- use_module(library(sandbox)).
:- use_module(library(socket)).
:- use_module(library(time)).
 :- use_module(library(debug)).
:- use_module(library(error)).
:- use_module(library(option)).
:- use_module(library(thread_pool)).
:- use_module(library(gensym)).

:- use_module(library(http/html_write)).

:- use_module(library(memfile)).

:- use_module(http_cgi).    

:- include(psp_header).
:- include(psp_session).
:- include(psp_json).
:- include(psp_handler).
:- include(psp_load_files).
:- include(psp_parser).
:- include(psp_messages).
:- include(ws_handler).
:- include(ws_rpc).




    

