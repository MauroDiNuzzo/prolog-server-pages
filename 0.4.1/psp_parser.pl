/*  psp_parser.pl
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
 

http_psp_parse(DOM, Codes) :-
    with_output_to(codes(Output), http_psp_parse_dom(DOM)),
    append([":-\n", Output, "true."], Codes).

http_psp_parse_dom([]) :- !.	
http_psp_parse_dom([element(Tag, Aux, Elements)|DOM]) :- 
    downcase_atom(Tag, html),
    select('psp:prolog'=Prolog, Aux, Attributes), 
    \+ normalize_space(atom(''), Prolog), !,
    format('~w, ', [Prolog]),    
    format('cgi_format(\'<~w\', []), ', [Tag]),    
    http_psp_attributes(Attributes),
    format('cgi_format(\'>\', []), ', []),
    http_psp_parse_dom(Elements),
    format('cgi_format(\'</~w>\', []), ', [Tag]),
    http_psp_parse_dom(DOM).
http_psp_parse_dom([element(Tag, Aux, Elements)|DOM]) :- 
    select('psp:prolog'=Prolog, Aux, Attributes), 
    \+ normalize_space(atom(''), Prolog), !,  
    format('cgi_format(\'<~w\', []), ', [Tag]),    
    http_psp_attributes(Attributes),
    format('cgi_format(\'>\', []), ', []),
    format('( ~w, ', [Prolog]),
    http_psp_parse_dom(Elements),
    format('fail ; true ), ', []),
    (	    is_html_void(Tag)
    ->  true
    ;	    format('cgi_format(\'</~w>\', []), ', [Tag])
    ),
    http_psp_parse_dom(DOM).
http_psp_parse_dom([element(Tag, Attributes, Elements)|DOM]) :- !,
    format('cgi_format(\'<~w\', []), ', [Tag]),    
    http_psp_attributes(Attributes),
    format('cgi_format(\'>\', []), ', []),
    http_psp_parse_dom(Elements),
    (	    is_html_void(Tag)
    ->  true
    ;	    format('cgi_format(\'</~w>\', []), ', [Tag])
    ),
    http_psp_parse_dom(DOM).
http_psp_parse_dom([Atom|DOM]) :-
    http_psp_layout(Atom, Normalized),
    (	  normalize_space(atom(''), Normalized)
    ->  true
    ;   format('cgi_format(~q, []), ', [Normalized])
    ),
    http_psp_parse_dom(DOM).
	
is_html_void(Tag) :-	
    % HTML void elements (http://www.w3.org/TR/html-markup/syntax.html#void-element)
    memberchk(Tag, [area, base, br, col, command, embed, hr, img, input, keygen, link, meta, param, source, track, wbr]).
	
	
    
http_psp_attributes([]) :- !.
http_psp_attributes([Qualified=Prolog|Attributes]) :- 
    atom_concat('psp:', Name, Qualified), !,
    format('(with_output_to(atom(_), (~w)) -> (cgi_format(\' ~w="\', []), call((~w)), cgi_format(\'"\', [])) ; true), ', [Prolog, Name, Prolog]),    
    http_psp_attributes(Attributes).    
http_psp_attributes([Name=Value|Attributes]) :- !,
    format(atom(Atom), '~q', [Value]),
    (   sub_atom(Atom, 0, 1, _, '\'')
    -> sub_atom(Atom, 1, _, 1, QValue)
    ;   QValue = Value
    ),
    format('cgi_format(\' ~w="~w"\', []), ', [Name, QValue]),
    http_psp_attributes(Attributes).
http_psp_attributes([Name|Attributes]) :-      % what happens to boolean attributes (sgml does not appear to process them properly!!!!)
    format('cgi_format(\' ~w\', []), ', [Name]),
    http_psp_attributes(Attributes).
    
    
		
http_psp_layout(Atom, Normalized) :-
    atom_codes(Atom, In),
    http_psp_layout_aux(In, Out),
    atom_codes(Normalized, Out). 
	
http_psp_layout_aux([], []) :- !.
http_psp_layout_aux([10|Codes], List) :- !, % newline
    format('~n', []),
    http_psp_layout_aux(Codes, List).
http_psp_layout_aux([9|Codes], List) :- !, % tab
    http_psp_layout_aux(Codes, List).
http_psp_layout_aux([Code|Codes], [Code|List]) :-
    http_psp_layout_aux(Codes, List).
			
			
	