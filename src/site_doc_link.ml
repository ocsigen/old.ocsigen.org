
open Eliom_pervasives

(** Extensions pour la gestion des liens dans la documentation web.

    Les mêmes extensions doivent être implémentées dans le générateur
    de pdf. *)

let register name f =
  Wiki_syntax.add_extension Wiki_syntax.wikicreole_parser name
    (Site_doc.wrap_phrasing name f);
  Wiki_syntax.add_extension Wiki_syntax.phrasing_wikicreole_parser name
    (Site_doc.wrap_phrasing name f);
  Wiki_syntax.add_extension Wiki_syntax.menu_parser name
    (Site_doc.wrap_phrasing name f)

(** Comment construire l'URL d'un identificateur OCaml dans l'API? *)

type id =
    string list * [ `Mod of string    | `ModType of string
                  | `Value of string  | `Type of string
		  | `Class of string  | `ClassType of string
		  | `Exc of string
		  | `Method of (string * string) | `Attr of (string * string)
		  | `Section of string
		  | `Index
		  | `IndexTypes
		  | `IndexExceptions
		  | `IndexValues
		  | `IndexAttributes
		  | `IndexMethods
		  | `IndexClasses
		  | `IndexClassTypes
		  | `IndexModules
		  | `IndexModuleTypes
		  ]

let path_of_id ?(prefix) id =
  let add_prefix s = match prefix with
  | None -> s
  | Some p -> p ^ s in
  match id with
  | (path, `Index) -> add_prefix "index"
  | (path, `IndexTypes) -> add_prefix "index_types"
  | (path, `IndexExceptions) -> add_prefix "index_exceptions"
  | (path, `IndexValues) -> add_prefix "index_values"
  | (path, `IndexAttributes) -> add_prefix "index_attributes"
  | (path, `IndexMethods) -> add_prefix "index_methods"
  | (path, `IndexClasses) -> add_prefix "index_classes"
  | (path, `IndexClassTypes) -> add_prefix "index_class_types"
  | (path, `IndexModules) -> add_prefix "index_modules"
  | (path, `IndexModuleTypes) -> add_prefix "index_module_types"
  | (path, `ModType name)
  | (path, `Mod name) ->
      String.concat "." (path @ [name])
  | (path, `ClassType name)
  | (path, `Class name) ->
      begin match prefix with
      | None -> String.concat "." (path @ [name]) ^ "-c"
      | Some p -> p ^ (String.concat "." (path @ [name]))
      end
  | (path, `Attr (cl, _))
  | (path, `Method (cl, _)) ->
      begin match prefix with
      | None -> String.concat "." (path @ [cl]) ^ "-c"
      | Some p -> p ^ String.concat "." (path @ [cl])
      end
  | (path, `Value _)
  | (path, `Type _)
  | (path, `Exc _)
  | (path, `Section _) ->
      add_prefix (String.concat "." path)

(* let filename_of_id id = path_of_id id ^ ".wiki" *)

let fragment_of_id : id -> string option = function
  | (_, `Value name) -> Some ("VAL" ^ name)
  | (_, `Type name) -> Some ("TYPE" ^ name)
  | (_, `Exc name) -> Some ("EXCEPTION" ^ name)
  | (_, `Attr (_, name)) -> Some ("ATTR" ^ name)
  | (_, `Method (_, name)) -> Some ("METHOD" ^ name)
  | (_, `Section name) -> Some name
  | _ -> None

(** Paramètres des extensions *)


let project_of_path p =
  Wiki_sql.get_wiki_info_by_pages p >|= fun i -> i.Wiki_types.wiki_id

let get_project bi args =
  (try
    project_of_path (List.assoc "project" args)
  with
  | Not_found ->
      Lwt.return bi.Wiki_widgets_interface.bi_wiki) >|=
  Site_doc.find_project

let get_project_and_version bi args =
  try
    (project_of_path (List.assoc "project" args)) >|= fun project ->
    let version = try Some (List.assoc "version" args) with Not_found -> None in
    (project, version)
  with
  | Not_found ->
     Lwt.return (bi.Wiki_widgets_interface.bi_wiki,
                 Some (Site_doc.guess_version ()))


let get_project_version bi args =
  get_project_and_version bi args >|= Site_doc.find_version

let get_src bi args =
  try Lwt.return (List.assoc "src" args)
  with Not_found -> Lwt.fail (Site_doc.Error "no \"src\" option.")

let get_chapter bi args =
  try Lwt.return (Neturl.split_path (List.assoc "chapter" args))
  with Not_found -> Lwt.return [""]

let get_opt_fragment bi args =
  Lwt.return
    (try Some (List.assoc "fragment" args)
     with Not_found -> None)

let get_text ~default bi args =
  Lwt.return
    (try List.assoc "text" args
     with Not_found -> default)

let get_subproject bi version args =
  Lwt.return
    (try String.split '/' (List.assoc "subproject" args)
     with Not_found ->
       match version.Site_doc.branch.Site_doc.br_subprojects with
	 | [] -> []
	 | x :: _ -> x)


(** Le contenu de <<pdfonly >> est ignoré. *)

let do_pdfonly bi args contents =
  Wikicreole.Phrasing_without_interactive (Lwt.return [])

let _ =
  Wiki_syntax.add_extension Wiki_syntax.wikicreole_parser "pdfonly" do_pdfonly




(** Le contenu de <<webonly >> est inclus tel quel. *)

let do_webonly bi args contents =
  match contents with
  | None ->  Wikicreole.Phrasing_without_interactive (Lwt.return [])
  | Some contents ->
      let contents =
	Wiki_syntax.xml_of_wiki Wiki_syntax.wikicreole_parser bi contents in
      Wikicreole.Flow5 contents

let _ =
  Wiki_syntax.add_extension Wiki_syntax.wikicreole_parser "webonly" do_webonly





(** Lien vers un chapitre du manuel :

    <<a_manual chapter=""
               [fragment="..."]
               [project="..."]
               [version="..."]
      | phrasing_without_interactives >>

 *)

let do_manual_link bi args contents =

  (* Get arguments *)
  lwt version = get_project_version bi args in
  lwt chapter = get_chapter bi args in
  lwt fragment = get_opt_fragment bi args in

  (* Parse contents *)
  lwt contents =
    match contents with
    | Some contents -> Wiki_syntax.phrasing_without_interactive_of_wiki bi contents
    | None -> Lwt.fail (Site_doc.Error "Empty contents") in

  (* Check file existence *)
  (*  ignore(version.Site_doc.manual_resolver chapter
      ~sp:bi.Wiki_widgets_interface.bi_sp); *)

  (* Build URL *)
  let doc_class = "ocsforge_doclink_" ^ version.Site_doc.branch.Site_doc.br_project.Site_doc.path in
  let a =
    Eliom_output.Html5.a
      ~a:[HTML5.M.a_class [doc_class]]
      ~service:(version.Site_doc.manual_service chapter)
      ?fragment contents ()
  in
  Lwt.return [a]

let _ = register "a_manual" do_manual_link
(* let _ = register_inline "a_manual" do_manual_link *)



(** <<a_file src="..."
             [project="..."]
             [version="..." ]
      | [phrasing_without_interactives] >>

    Lien vers un fichier d'exemple. Ce fichier doit être inclus
    dans le dépôt darcs de la documentation. Si le 'contents' de
    l'extension est vide, le corps du lien est le nom du fichier. *)

let do_aux_link bi args contents =

  (* Get arguments *)
  lwt src = get_src bi args in
  lwt version = get_project_version bi args in
  lwt fragment = get_opt_fragment bi args  in

  (* Check file existence *)
  (*  ignore(version.Site_doc.aux_resolver (Neturl.split_path src)
	   ~sp:bi.Wiki_widgets_interface.bi_sp); *)

  (* Parse contents *)
  lwt contents = match contents with
   | None -> Lwt.return [HTML5.M.pcdata (Filename.basename src)]
   | Some contents -> Wiki_syntax.phrasing_without_interactive_of_wiki bi contents in

  (* Build URL *)
  let doc_class = "ocsforge_doclink_" ^ version.Site_doc.branch.Site_doc.br_project.Site_doc.path in
  let a =
    Eliom_output.Html5.a
      ~a:[HTML5.M.a_class [doc_class]]
      ~service:(version.Site_doc.aux_service (Neturl.split_path src))
      ?fragment contents	() in
  Lwt.return [a]

let _ = register "a_file" do_aux_link

let do_aux_img bi args contents =

  (* Get arguments *)
  lwt src = get_src bi args in
  lwt version = get_project_version bi args in
  lwt fragment = get_opt_fragment bi args in

  (* Check file existence *)
  (*  ignore(version.Site_doc.aux_resolver (Neturl.split_path src)
	   ~sp:bi.Wiki_widgets_interface.bi_sp); *)

  (* Parse contents *)
  let alt = match contents with
  | None -> Filename.basename src
  | Some contents -> contents in

  (* Build URL *)
  let src =
    Eliom_output.Html5.make_string_uri
      ~service:(version.Site_doc.aux_service (Neturl.split_path src)) () in
  Lwt.return [ HTML5.M.img ~src ~alt () ]

let _ = register "a_img" do_aux_img

let do_aux_script bi args contents =

  (* Get arguments *)
  lwt src = get_src bi args in
  lwt version = get_project_version bi args in
  lwt fragment = get_opt_fragment bi args in

  (* Check file existence *)
  (* ignore(version.Site_doc.aux_resolver (Neturl.split_path src) *)
	   (* ~sp:bi.Wiki_widgets_interface.bi_sp); *)

  (* Build URL *)
  let typ = "text/javascript" in
  let src =
    Eliom_output.Html5.make_string_uri
      ~service:(version.Site_doc.aux_service (Neturl.split_path src)) () in
  Lwt.return [ HTML5.M.script
		 ~a:[ HTML5.M.a_mime_type typ; HTML5.M.a_src src ]
		 (HTML5.M.cdata_script "") ]

let _ = register "a_script" do_aux_script



(** <<a_api [project="..."] [branch="..."] [text="..."]
      | val id >>

    Lien vers l'API.

*)

let is_capitalized s =
  String.length s >= 1 &&
  let c = int_of_char s.[0] in
  int_of_char 'A' <= c && c <= int_of_char 'Z'
let is_all_capital s = String.uppercase s = s

let check_capitalized_path path =
  List.iter
    (fun name -> if not (is_capitalized name) then
      raise (Site_doc.Error (Printf.sprintf "%S is not a valid module name" name)))
    path

let parse_lid id =
  match List.rev (String.split '.' (String.concat "" id)) with
  | id :: rpath when not (is_capitalized id) ->
      check_capitalized_path rpath;
      (List.rev rpath, id)
  | _ -> raise (Site_doc.Error (Printf.sprintf "invalid ocaml id %S" (String.concat "" id)))

let parse_uid id =
  match List.rev (String.split '.' (String.concat "" id)) with
  | id :: rpath when is_capitalized id ->
      check_capitalized_path rpath;
      (List.rev rpath, id)
  | _ -> raise (Site_doc.Error (Printf.sprintf "invalid ocaml id %S" (String.concat "" id)))

let parse_method id =
  match String.split '#' id with
  | [id; mid] when not (is_capitalized id) && not (is_capitalized id) -> (id, mid)
  | _ -> raise (Site_doc.Error (Printf.sprintf "invalid method name %S" id))

let parse_contents bi args contents =
  match contents with
  | None | Some "" -> raise (Site_doc.Error "contents must be an Ocaml id")
  | Some def ->
      let def = String.split ~multisep:true ' ' def in
      let def = List.flatten (List.map (String.split ~multisep:true '\n') def) in
      match def with
      | "intro" :: [] -> [], `Index
      | "index" :: [] -> [], `Index
      | "index" :: "types" :: _ -> [], `IndexTypes
      | "index" :: "exceptions" :: _ -> [], `IndexExceptions
      | "index" :: "values" :: _ -> [], `IndexValues
      | "index" :: "attributes" :: _ -> [], `IndexAttributes
      | "index" :: "methods" :: _ -> [], `IndexMethods
      | "index" :: "classes" :: _ -> [], `IndexClasses
      | "index" :: "class" :: "types" :: _ -> [], `IndexClassTypes
      | "index" :: "modules" :: _ -> [], `IndexModules
      | "index" :: "module" :: "types" :: _ -> [], `IndexModuleTypes
      | "val":: lid | "value":: lid ->
	  let path, id = parse_lid lid in
	  path, `Value id
      | "type":: lid ->
	  let path, id = parse_lid lid in
	  path, `Type id
      | "class":: "type" :: lid ->
	  let path, id = parse_lid lid in
	  path , `ClassType id
      | "class":: lid ->
	  let path, id = parse_lid lid in
	  path , `Class id
      | "module":: "type" :: uid | "mod":: "type" :: uid ->
	  let path, id = parse_uid uid in
	  path, `ModType id
      | "module":: uid | "mod":: uid ->
	  let path, id = parse_uid uid in
	  path, `Mod id
      | "exception":: uid | "exc":: uid ->
	  let path, id = parse_uid uid in
	  path, `Exc id
      | "attribute":: lid | "attr":: lid ->
	  let path, id = parse_lid lid in
	  let id, did = parse_method id in
	  path, `Attr (id, did)
      | "method":: lid ->
	  let path, id = parse_lid lid in
	  let id, mid = parse_method id in
	  path, `Method (id, mid)
      | "section":: lid ->
	  let path, id = parse_lid lid in
	  path, `Section id
      | _ -> raise (Site_doc.Error "invalid contents")

let string_of_id : id -> string = function
  | (path, ( `Method (cl,name) | `Attr (cl, name))) ->
      name ^ " [" ^ String.concat "." (path @ [cl]) ^"]"
  | (path, ( `Mod name   | `ModType name | `Class name | `ClassType name
           | `Value name | `Type name   | `Exc name )) ->
      String.concat "." (path @ [name])
  | (_,`Index) -> "Api introduction"
  | (_, `IndexTypes)
  | (_, `IndexExceptions)
  | (_, `IndexValues)
  | (_, `IndexAttributes)
  | (_, `IndexMethods)
  | (_, `IndexClasses)
  | (_, `IndexClassTypes)
  | (_, `IndexModules)
  | (_, `IndexModuleTypes)
  | (_,`Section _) -> ""

let do_api_link prefix bi args contents =

  (* Get arguments *)
  lwt version = get_project_version bi args in
  lwt sub = get_subproject bi version args in
  let id = parse_contents bi args contents in
  lwt body = get_text ~default:(string_of_id id) bi args in

  (* Check file existence *)
  (* ignore(version.Site_doc.api_resolver (sub @ [path_of_id ?prefix id]) *)
	   (* ~sp:bi.Wiki_widgets_interface.bi_sp); *)

  (* Build URL *)
  let doc_class = "ocsforge_doclink_" ^ version.Site_doc.branch.Site_doc.br_project.Site_doc.path in
  let a =
    Eliom_output.Html5.a
      ~a:[HTML5.M.a_class [doc_class]]
      ~service:(version.Site_doc.api_service (sub @ [path_of_id ?prefix id]))
      ?fragment:(fragment_of_id id)
      [HTML5.M.pcdata body] () in
  Lwt.return [a]

let _ = register "a_api" (do_api_link None)
let _ = register "a_api_type" (do_api_link (Some "type_"))
let _ = register "a_api_code" (do_api_link (Some "code_"))
