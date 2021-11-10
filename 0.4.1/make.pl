/*  make.pl
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

:- set_prolog_flag(encoding, utf8).

:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_error)).
:- use_module(library(http/http_exception)).
:- use_module(library(http/http_log)).   
:- use_module(library(http/http_mime_plugin)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_session)).
:- use_module(library(http/http_stream)).

:- use_module(cgi_httpd).
:- use_module(http_cgi).
:- use_module(http_psp).  

:- http_handler(root(.), psp_handler, [prefix, time_limit(30)]).


:- 
  write('\n\n\n\nProlog Server Pages (C) 2015 Mauro DiNuzzo\n'), 
  absolute_file_name(app_data('../Prolog Server Pages'), AppData),
  assert(user:file_search_path(http_app_data, AppData)),
  assert(user:file_search_path(http_temp, http_app_data('Temp'))),
  assert(user:file_search_path(http_session_data, http_app_data('SessionData'))),  
  assert(user:file_search_path(http_logs, http_app_data('Logs'))),
  absolute_file_name(http_logs(httpd), LogFile, [extensions([log])]),
  catch(set_setting(http:logfile, LogFile), _, true),
  prolog_load_context(directory, Directory),
  absolute_file_name('../bin', Bin, [relative_to(Directory), file_type(directory)]),
  working_directory(_, Bin),
  qsave_program('psp.exe', [
    autoload(true),
    class(kernel),
    emulator('swipl.exe'), 
    stand_alone(true), 
    goal(http_server(http_dispatch, [port(_), workers(1), timeout(30)]))
  ]),
  write('Ok\n'),
  halt.
