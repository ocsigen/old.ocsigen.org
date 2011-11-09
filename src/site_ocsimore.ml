(* Ocsigen web site
 * Copyright (C) 2009 Vincent Balat
 *
 * This program is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
 *)

open Eliom_pervasives

(** Extension wikifile *)

let repository_default    = "/var/www/data/darcs"
let repository_restricted = "/var/www/data/restricted"

let get_repository bi args =
  try match List.assoc "repository" args with
  | "restricted" -> repository_restricted
  | _            -> repository_default
  with Not_found -> repository_default

let do_wikifile bi args c =
  `Flow5
    (try
      let repository = get_repository bi args in
      let filename = List.assoc "file" args in
      let file = Ocsigen_local_files.resolve
	  ~no_check_for:repository
	  ~request:(Eliom_request_info.get_request ())
	  ~filename:(repository ^ "/" ^ filename) in
      match file with
      | Ocsigen_local_files.RFile file ->
	Lwt_io.with_file ~mode:Lwt_io.input file
	  (fun ch ->
	    lwt data = Lwt_io.read ch in
	    lwt xml = Wiki_syntax.xml_of_wiki
	      (Wiki_syntax.cast_wp Wiki_syntax.wikicreole_parser_without_header_footer) bi data in
	    Lwt.return xml)
      | Ocsigen_local_files.RDir file -> Lwt.return []
    with
    | Not_found -> Lwt.return []
    | exc ->
        Lwt.return
	  [ HTML5.M.div
	      ~a:[HTML5.M.a_class ["error"]]
	      [ HTML5.M.pcdata (Printexc.to_string exc ) ] ])

let _ =
  Wiki_syntax.register_interactive_simple_flow_extension
    ~name:"wikifile" ~reduced:false do_wikifile

(** Extension script *)

let do_script bi args c =
  `Flow5
    (try
      let script = List.assoc "src" args in
      Lwt.return
	[ HTML5.M.script
	    ~a:[ HTML5.M.a_mime_type "text/javascript";
		 HTML5.M.a_src script]
	    (HTML5.M.cdata_script "") ]
    with Not_found ->
      let content =
        match c with
        | Some c -> c
        | None -> ""
      in
      Lwt.return
	[ HTML5.M.script
	    ~a:[HTML5.M.a_mime_type "text/javascript"]
	    (HTML5.M.cdata_script content) ])

let _ =
  Wiki_syntax.register_simple_flow_extension
    ~name:"script" ~reduced:false do_script

(** Extension atom *)
(* Mmmmh. à revoir complètement *)

let atom_header =
  Page_site.Header.create_header
    (fun () ->
      [ HTML5.M.link
	  ~rel:[`Alternate]
	  ~href:"http://ocsigen.org/news.atom"
	  ~a:[ HTML5.M.a_title "Ocsigen news";
	       HTML5.M.a_mime_type "application/atom+xml"; ]
	  () ])

let add_atom_header () = Page_site.Header.require_header atom_header

let do_atom bi args c =
  add_atom_header ();
  `Flow5 (Lwt.return [])

let _ =
  Wiki_syntax.register_simple_flow_extension ~name:"atom" ~reduced:false do_atom


(* Work in progress *)

let get_inline bi args =
  List.mem_assoc "inline" args

let do_wip bi args xml =
  `Flow5
    (lwt xml = match xml with
      | Some c -> (c :> HTML5_types.flow5 HTML5.M.elt list Lwt.t)
      | None -> Lwt.return [] in
     Lwt.return
       [ HTML5.M.aside
           ~a:[HTML5.M.a_class ["wip"]]
           ( HTML5.M.header [HTML5.M.h5 [HTML5.M.pcdata "Work in progress"]]
	     :: xml ) ])

let do_wip_inline bi args xml =
  `Phrasing_without_interactive
    (lwt xml = match xml with
       | Some c -> (c :> HTML5_types.phrasing HTML5.M.elt list Lwt.t)
       | None -> Lwt.return [] in
     let title =
       try List.assoc "title" args
       with Not_found -> "WIP: " in
     Lwt.return
       [ HTML5.M.span
           ~a:[HTML5.M.a_class ["wip"]]
           ( HTML5.M.strong [HTML5.M.pcdata title] :: xml ) ])

let _ =
  Wiki_syntax.register_wiki_flow_extension ~reduced:false ~name:"wip" { Wiki_syntax.fpp = do_wip };
  Wiki_syntax.register_wiki_phrasing_extension ~reduced:false ~name:"wip-inline" { Wiki_syntax.ppp = do_wip_inline }


(* Concepts *)

let do_concepts bi args xml =
  `Flow5
    (lwt xml = match xml with
      | Some c -> (c :> HTML5_types.flow5 HTML5.M.elt list Lwt.t)
      | None -> Lwt.return [] in
     Lwt.return
       [ HTML5.M.aside
           ~a:[HTML5.M.a_class ["concepts"]]
           ( HTML5.M.header [HTML5.M.h5 [HTML5.M.pcdata "Concepts"]]
	     :: xml ) ])

let _ =
  Wiki_syntax.register_wiki_flow_extension
    ~name:"concepts" ~reduced:false { Wiki_syntax.fpp = do_concepts }


(* Concept *)

let get_title bi args =
  try List.assoc "title" args
  with Not_found -> "Concept"

let do_concept bi args xml =
  `Flow5
    (lwt xml = match xml with
      | Some c -> (c :> HTML5_types.flow5 HTML5.M.elt list Lwt.t)
      | None -> Lwt.return [] in
     let title = get_title bi args in
     Lwt.return
       [ HTML5.M.aside
           ~a:[HTML5.M.a_class ["concept"]]
           ( HTML5.M.header [HTML5.M.h5 [HTML5.M.span
					   ~a:[HTML5.M.a_class ["concept_prefix"]]
					   [HTML5.M.pcdata "Concept: "];
					 HTML5.M.pcdata title]]
	     :: xml ) ])

let _ =
  Wiki_syntax.register_wiki_flow_extension
    ~name:"concept" ~reduced:false { Wiki_syntax.fpp = do_concept }


