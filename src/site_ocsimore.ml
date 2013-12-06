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

open Eliom_content
open Eliom_lib
open Lwt_ops

(*****************************************************************************)
(** Extension wikifile *)

let repository_default    = "/var/www/data/git"
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
          ~filename:(repository ^ "/" ^ filename) () in
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
          [ Html5.F.div
              ~a:[Html5.F.a_class ["error"]]
              [ Html5.F.pcdata (Printexc.to_string exc ) ] ])

let _ =
  Wiki_syntax.register_interactive_simple_flow_extension
    ~name:"wikifile" ~reduced:false do_wikifile

(*****************************************************************************)
(** Extension script *)

let do_script bi args c =
  `Flow5
    (try
      let script = List.assoc "src" args in
      Lwt.return
        [ Html5.F.script
            ~a:[ Html5.F.a_mime_type "text/javascript";
                 Html5.F.a_src (Xml.uri_of_string script)]
            (Html5.F.cdata_script "") ]
    with Not_found ->
      let content =
        match c with
        | Some c -> c
        | None -> ""
      in
      Lwt.return
        [ Html5.F.script
            ~a:[Html5.F.a_mime_type "text/javascript"]
            (Html5.F.cdata_script content) ])

let _ =
  Wiki_syntax.register_simple_flow_extension
    ~name:"script" ~reduced:false do_script

(*****************************************************************************)
(** Extension atom *)
(* Mmmmh. à revoir complètement *)

let atom_header =
  Page_site.Header.create_header
    (fun () ->
      [ Html5.F.link
          ~rel:[`Alternate]
          ~href:(Xml.uri_of_string "http://ocsigen.org/news.atom")
          ~a:[ Html5.F.a_title "Ocsigen news";
               Html5.F.a_mime_type "application/atom+xml"; ]
          () ])

let add_atom_header () = Page_site.Header.require_header atom_header

let do_atom bi args c =
  `Flow5 (
    lwt () = add_atom_header () in
    Lwt.return []
  )

let _ =
  Wiki_syntax.register_simple_flow_extension ~name:"atom" ~reduced:false do_atom


(*****************************************************************************)
(** Extension wip *)
(* Work in progress *)

let get_inline bi args =
  List.mem_assoc "inline" args

let do_wip bi args xml =
  `Flow5
    (lwt xml = match xml with
      | Some c -> (c :> Html5_types.flow5 Html5.F.elt list Lwt.t)
      | None -> Lwt.return [] in
     Lwt.return
       [ Html5.F.aside
           ~a:[Html5.F.a_class ["wip"]]
           ( Html5.F.header [Html5.F.h5 [Html5.F.pcdata "Work in progress"]]
             :: xml ) ])

let do_wip_inline bi args xml =
  `Phrasing_without_interactive
    (lwt xml = match xml with
       | Some c -> (c :> Html5_types.phrasing Html5.F.elt list Lwt.t)
       | None -> Lwt.return [] in
     let title =
       try List.assoc "title" args
       with Not_found -> "WIP: " in
     Lwt.return
       [ Html5.F.span
           ~a:[Html5.F.a_class ["wip"]]
           ( Html5.F.strong [Html5.F.pcdata title] :: xml ) ])

let _ =
  Wiki_syntax.register_wiki_flow_extension ~reduced:false ~name:"wip" { Wiki_syntax.fpp = do_wip };
  Wiki_syntax.register_wiki_phrasing_extension ~reduced:false ~name:"wip-inline" { Wiki_syntax.ppp = do_wip_inline }


(*****************************************************************************)
(** Extension Concepts *)

let do_concepts bi args xml =
  `Flow5
    (lwt xml = match xml with
      | Some c -> (c :> Html5_types.flow5 Html5.F.elt list Lwt.t)
      | None -> Lwt.return [] in
     Lwt.return
       [ Html5.F.aside
           ~a:[Html5.F.a_class ["concepts"]]
           ( Html5.F.header [Html5.F.h5 [Html5.F.pcdata "Concepts"]]
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
      | Some c -> (c :> Html5_types.flow5 Html5.F.elt list Lwt.t)
      | None -> Lwt.return [] in
     let title = get_title bi args in
     Lwt.return
       [ Html5.F.aside
           ~a:[Html5.F.a_class ["concept"]]
           ( Html5.F.header [Html5.F.h5 [Html5.F.span
                                           ~a:[Html5.F.a_class ["concept_prefix"]]
                                           [Html5.F.pcdata "Concept: "];
                                         Html5.F.pcdata title]]
             :: xml ) ])

let _ =
  Wiki_syntax.register_wiki_flow_extension
    ~name:"concept" ~reduced:false { Wiki_syntax.fpp = do_concept }


(*****************************************************************************)
(* Extension paragraph *)

let do_paragraph bi args xml =
  `Flow5
    (lwt xml = match xml with
       | Some c -> (c :> _ Html5.F.elt list Lwt.t)
       | None -> Lwt.return [] in
     Lwt.return
       [ Html5.F.div
           ~a:[Html5.F.a_class ["paragraph"]]
          xml ])

let _ =
  Wiki_syntax.register_wiki_flow_extension
    ~reduced:false
    ~name:"paragraph"
    { Wiki_syntax.fpp = do_paragraph }



(*****************************************************************************)
(** Extension Client/Server-Switch *)


let do_client_server_switch bi args xml =
  let html5 =
    let wiki, path = Wiki_self_services.get_wiki_page_for_path (Eliom_request_info.get_current_sub_path ()) in
    let path =
      let rec aux sofar = function
        | [] -> None
        | "api" :: "client" :: rest -> Some (List.rev sofar @ "api" :: "server" :: rest, "client", "server")
        | "api" :: "server" :: rest -> Some (List.rev sofar @ "api" :: "client" :: rest, "server", "client")
        | head :: tail -> aux (head :: sofar) tail
      in aux [] path
    in
    match path with
      | Some (path, src, trg) ->
          let service =
            let s =
              let open Wiki_self_services in
              Servpages.find Wiki_self_services.servpages bi.Wiki_widgets_interface.bi_wiki
            in
            Eliom_service.preapply s path in
          Html5.F.([
            div ~a:[a_class ["client-server-switch-wrapper"]] [
              div ~a:[a_class ["client-server-switch"]] [
                span ~a:[a_class [src; "source"]] [pcdata ("This is "^src^" API")];
                pcdata " (go to ";
                a ~a:[a_class [trg; "target"]] ~service [pcdata trg] ();
                pcdata ")";
              ]
            ]
          ])
      | None -> []
  in
  `Flow5 (Lwt.return html5)

let () =
  Wiki_syntax.register_wiki_flow_extension
    ~reduced:false
    ~name:"client-server-switch"
    { Wiki_syntax.fpp = do_client_server_switch }


(*****************************************************************************)
(** Extension google search *)

let gsearch_service =
  Eliom_service.external_service ~prefix:"https://google.com" ~path:["search"]
    ~get_params:(Eliom_parameter.string "q") ()

let before_gsearch_service =
  Eliom_service.coservice' ~name:"before_gsearch"
    ~get_params:(Eliom_parameter.string "q") ()

let _ =
  Eliom_registration.Redirection.register
    ~service:before_gsearch_service
    (fun q () ->
      Lwt.return
        (Eliom_service.preapply
	   ~service:gsearch_service (q^" site:ocsigen.org") ))

let do_google_search bi args xml =
  `Flow5
    (Lwt.return
       [ Html5.F.(get_form
           ~service:before_gsearch_service
           (fun tosearchfield ->
             [string_input
                 ~a:[a_placeholder "search ..."]
                 ~name:tosearchfield
                 ~input_type:`Text ();
              string_input ~input_type:`Submit ~value:"Search" ()
	     ]
	   ))
       ])

let _ =
  Wiki_syntax.register_wiki_flow_extension
    ~name:"googlesearch" ~reduced:false { Wiki_syntax.fpp = do_google_search }
