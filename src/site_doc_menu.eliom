
{shared{
  open Eliom_lib
  open Eliom_content
  open Eliom_lib.Lwt_ops
}}


let register name f =
  Wiki_syntax.register_interactive_simple_flow_extension
    ~name ~reduced:false (Site_doc.wrap_flow5 name f)

(** <<doctree [project="..."] [subproject="..."] [version="..."] >> *)

let build_api_tree bi version file =
  try_lwt
  Wiki_menu.build_tree_from_file bi
    ~create_service:(fun ?wiki file ->
      (version.Site_doc.api_service file :> Eliom_tools.get_page))
    ~file
  with ee ->
    Printf.printf "exc during api_tree %s %s\n%!" (Ocsigen_local_files.(match file with
        | RFile s -> s
        | RDir s -> s)) (Printexc.to_string ee);
    raise Not_found

let build_api_trees bi version =
  try_lwt
    (Lwt_list.map_p
       (build_api_tree bi version)
       (version.Site_doc.api_menu ())) >|= fun l ->
    List.concat l
  with
  | Wiki_dir.Undefined | Ocsigen_local_files.Failed_404 -> Lwt.return []
  | exc ->
    let msg =
      Format.sprintf "Can't read api menu (exception: %s)"
	      (Printexc.to_string exc) in
    Lwt.fail (Site_doc.Error msg)

let build_manual_tree bi branch =
    try_lwt
      Wiki_menu.build_tree_from_file bi
	~file:(branch.Site_doc.manual_menu ())
	~create_service:(fun ?wiki file ->
	  (branch.Site_doc.manual_service file :> Eliom_tools.get_page))
    with
    | Wiki_dir.Undefined | Ocsigen_local_files.Failed_404 -> Lwt.return []
    | exc ->
      let msg =
	Format.sprintf "Can't read manual menu (exception: %s)"
	  (Printexc.to_string exc) in
      Lwt.fail (Site_doc.Error msg)

let build_doctree ?service bi version =
  build_api_trees bi version >>= fun api_tree ->
  build_manual_tree bi version >>= fun manual_tree ->
  Lwt.return (Eliom_tools.D.hierarchical_menu_depth_first
                ~classe:["doctree"; "manualapitree"]
		(Eliom_tools.Not_clickable, manual_tree @ api_tree)
		~whole_tree:true
		?service
		())

let build_apitree ?service bi version =
  build_api_trees bi version >>= fun api_tree ->
  Lwt.return (Eliom_tools.D.hierarchical_menu_depth_first
                ~classe:["doctree"; "apitree"]
		(Eliom_tools.Not_clickable, api_tree)
		~whole_tree:true
		?service
		())

let build_manualtree ?service bi version =
  build_manual_tree bi version >>= fun manual_tree ->
  Lwt.return (Eliom_tools.D.hierarchical_menu_depth_first
                ~classe:["doctree"; "manualtree"]
		(Eliom_tools.Not_clickable, manual_tree)
		~whole_tree:true
		?service
		())

let do_doctree bi args contents =
  Site_doc_link.get_project_version bi args >>= build_doctree bi

let do_apitree bi args contents =
  Site_doc_link.get_project_version bi args >>= build_apitree bi

let do_manualtree bi args contents =
  Site_doc_link.get_project_version bi args >>= build_manualtree bi


let _ = register "doctree" do_doctree
let _ = register "apitree" do_apitree
let _ = register "manualtree" do_manualtree

(** <<docmenu project='...' level='2' >> *)
(** <<docversions project='...' level='2' >> *)

let get_relative_path_in_wiki bi project service =
  let prefix = project.Site_doc.path in
  let url =
    Html5.D.make_string_uri ~absolute_path:true ~service () in
(*  assert(String.length prefix < String.length url);  *)
  assert(String.sub url 1 (String.length prefix) = prefix);
  String.sub url
    (String.length prefix + 2)
    (String.length url - String.length prefix - 2)

let build_doc b bi level project =
  let wiki_id = Wiki_types.string_of_wiki project.Site_doc.wiki in
  let last_stable_version = Site_doc.get_last_version project in
  let last_stable_manual = last_stable_version.Site_doc.manual_service [""] in
  Format.bprintf b
    "%s[[wiki(%s):%s|Doc]]\n"
    level wiki_id (get_relative_path_in_wiki bi project last_stable_manual);
  let version = Site_doc.(find_version (project.wiki, Some (guess_version ()))) in
  let wiki_id = Wiki_types.string_of_wiki project.Site_doc.wiki in
  let manual = version.Site_doc.manual_service [""] in
  let api = version.Site_doc.api_service [""] in
  Format.bprintf b
    ("=%s[[wiki(%s):%s|Manual]]\n" ^^
     "=%s[[wiki(%s):%s|API Documentation]]\n")
    level wiki_id (get_relative_path_in_wiki bi project manual)
    level wiki_id (get_relative_path_in_wiki bi project api)

let get_level bi args =
  try int_of_string (List.assoc "level" args)
  with _ -> 1

let do_docmenu bi args contents =
  let level = get_level bi args in
  Site_doc_link.get_project bi args >>= fun project ->
  let b = Buffer.create 256 in
  build_doc b bi (String.make level '=') project;
  let contents =
    match contents with
    | Some c -> Buffer.contents b ^ c
    | None -> Buffer.contents b in
  Wiki_syntax.xml_of_wiki (Wiki_syntax.cast_wp Wiki_syntax.menu_parser) bi contents

let register name f =
  let error (msg:string) =
    Lwt.return
      [ Html5.F.h1
	  [ Html5.F.span ~a:[Html5.F.a_class ["doclink_error"]] [Html5.F.pcdata msg]] ] in
  let wrap f = fun bi args contents ->
    `Flow5
      (try_lwt
	  f bi args contents
       with
       | Site_doc.Project_not_found -> Lwt.return []
       | Site_doc.Error msg -> error (Format.sprintf "Error %s: %s" name msg)
       | exc ->
	 error (Format.sprintf "Error %s: exception %s" name
		  (Printexc.to_string exc) ) ) in

  Wiki_syntax.register_simple_extension Wiki_syntax.menu_parser name (wrap f)

let _ = register "docmenu" do_docmenu

{shared{
  let version_path_custom_data =
    Html5.Custom_data.create_json ~name:"version_path" Json.t<string list>
}}

{server{
  let parse_manual_api_path project =
    let rec aux prefix = function
      | [] -> None
      | ("manual"|"api" as ma) :: rest ->
        Some (List.rev prefix, None, ma, rest)
      | "dev" :: (("manual" | "api" as ma) :: rest) ->
        Some (List.rev prefix, Some Version.Dev, ma, rest)
      | (version :: (("manual" | "api" as ma) :: rest)) as all ->
        begin try
            let version = Version.parse version in
            if List.exists (fun v -> version = v.Site_doc.version) project.Site_doc.versions
            then
              Some (List.rev prefix, Some version, ma, rest)
            else raise Not_found
          with _ -> aux (List.hd all :: prefix) (List.tl all)
        end
      | snippet :: rest -> aux (snippet :: prefix) rest
    in aux []
}}

let version_options project (prefix, current_version, manual_api, rest) =
  let open Html5.F in
  let selected ?(default=false) v =
    if (default && current_version == None) || current_version = Some v
    then [ a_selected `Selected ]
    else []
  in
  let version_path version =
    Html5.Custom_data.attrib version_path_custom_data
      (prefix @ Option.to_list version @ manual_api :: rest)
  in
  let latest,versions =
    match project.Site_doc.last_stable with
      | None -> [],project.Site_doc.versions
      | Some last_stable ->
        let v = last_stable.Site_doc.version in
        let versions = List.filter (fun v' -> v'.Site_doc.version <> v) project.Site_doc.versions in
        [option
            ~a:(a_value "" :: version_path None :: selected ~default:true v)
            (Printf.ksprintf pcdata "Latest stable (%s)" (Version.to_string v))], versions
  in
  let dev =
    try
      match project.Site_doc.dev_version with
      | None -> None
      | Some dev ->
        let v = dev.Site_doc.version in
        let name = Version.to_string v in
        let opt = option ~a:(a_value name :: version_path (Some name) :: selected v)
            (pcdata "Development") in
        Some opt
    with Not_found -> None
  in
  let releases =
    let all = List.map
        (fun version ->
           let v = version.Site_doc.version in
           let name = Version.to_string v in
           Version.major v,
           option ~a:(a_value name :: version_path (Some name) :: selected v) (pcdata name) ) versions in
    let by_major = List.fold_right (fun (major,opt) acc ->
        match acc with
        | []-> [major,[opt]]
        | (cmajor,l)::xs when cmajor = major -> (cmajor,(opt::l))::xs
        | l -> (major,[opt])::l) all [] in
    match by_major with
    | [] -> []
    | [_,l] -> [optgroup ~label:"Releases" l]
    | groups -> List.map (fun (major,l) -> optgroup ~label:(Printf.sprintf "Releases: versions %s.*" major) l) groups
  in
  latest @ Option.to_list dev @ releases

let do_docversion bi args _ =
  let change_version = {{
    fun ev ->
      Js.Opt.iter (ev##currentTarget) (fun target ->
        Js.Opt.iter (Dom_html.CoerceTo.select target) (fun select ->
          Js.Opt.iter (select##options##item(select##selectedIndex)) (fun option ->
            try
              let path =
                Html5.Custom_data.get_dom (Dom_html.element option)
                  version_path_custom_data
              in
              let path = String.concat "/" (List.map Url.encode path) in
              Dom_html.window##location##pathname <- Js.string path
            with Not_found -> ())))
  }} in
  `Flow5
    (lwt project = Site_doc_link.get_project bi args in
     Lwt.return
       (match parse_manual_api_path project (Eliom_request_info.get_current_full_path ()) with
         | None -> []
         | Some ((p, v, ma, r) as path) ->
           Html5.F.([
             div ~a:[a_class ["docversion"]] [
               label [pcdata "Version"];
               Raw.select ~a:[a_onchange change_version]
                 (version_options project path)
             ]])))

let do_manual_api_link manual_or_api bi args content =
  `Flow5
    (lwt project = Site_doc_link.get_project bi args in
     let contents =
       Printf.sprintf "=[[wiki(%s):%s/|%s]]"
         (Wiki_types.string_of_wiki project.Site_doc.wiki)
         (match parse_manual_api_path project (Eliom_request_info.get_current_full_path ()) with
           | Some (prefix, version, _, _) ->
             String.concat "/"
               (List.map Url.encode
                  (Option.to_list (match version with None -> None | Some v -> Some (Version.to_string v)) @ [manual_or_api]))
           | None ->
             manual_or_api)
         (Option.get (fun () -> manual_or_api) content)
     in
     Wiki_syntax.xml_of_wiki (Wiki_syntax.cast_wp Wiki_syntax.menu_parser) bi contents)


let _ =
  Wiki_syntax.register_simple_extension Wiki_syntax.wikicreole_parser "docversion" do_docversion;
  Wiki_syntax.register_simple_extension Wiki_syntax.menu_parser "manual-link"
    (do_manual_api_link "manual");
  Wiki_syntax.register_simple_extension Wiki_syntax.menu_parser "api-link"
    (do_manual_api_link "api")
