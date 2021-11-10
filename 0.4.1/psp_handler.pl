/*  psp_handler.pl
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
 
 
%% psp_handler(+Request)
%
% Handle Prolog Server Pages (PSP) files. This predicate is used by http_handler/3, which implies
% that the interface is kept compatible with the library(http/http_dispatch). In a typical CGI environment
% this is normally the only defined handler.

psp_handler(Request) :- 
    memberchk(session(SessionID), Request),
    absolute_file_name(http_logs(SessionID), LogFile, [extensions([log])]),
    set_setting(http:logfile, LogFile),
    memberchk(path_translated(File), Request),
    (   ws_handler(Request)
    -> true
    ;   user:load_files(File, []),
        http_cleanup(Request)
    ).
    

