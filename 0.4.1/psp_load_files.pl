/*  psp_load_files.pl
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
 
 :- dynamic user:prolog_load_file/2.
:- multifile user:prolog_load_file/2.

user:prolog_load_file(Spec, Options) :-
    strip_module(Spec, Module, Name),
    absolute_file_name(Name, File),    
    file_name_extension(_, psp, File), !,    
    working_directory(Old, Old),
    file_directory_name(File, New),
    working_directory(_, New),    
    load_html(File, DOM, [case_sensitive_attributes(false), case_preserving_attributes(true), dtd(DTD)]),    
    assert(http_cgi:current_dtd(DTD)),
    http_psp_parse(DOM, Codes),   
    http_current_request(Request),
    memberchk(session(SessionID), Request),
    absolute_file_name(http_temp(SessionID), TempFile, [extensions([pl])]),    
    setup_call_cleanup(
        open(TempFile, write, TempStream), 
        format(TempStream, '~s', [Codes]), 
        close(TempStream)
    ),       
    (   absolute_file_name(http_app_data('config.pl'), Config),  
        exists_file(Config)
    ->  Module:load_files(Config, [if(not_loaded)]) 
    ;   true
    ),    
    Module:load_files(TempFile, Options),
    working_directory(_, Old).  
    
    