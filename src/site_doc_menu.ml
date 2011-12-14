
let (>>=) = Lwt.bind
let (>|=) m f = Lwt.map f m

let register name f =
  Wiki_syntax.register_interactive_simple_flow_extension
    ~name ~reduced:false (Site_doc.wrap_flow5 name f)

(** <<doctree [project="..."] [subproject="..."] [version="..."] >> *)

let build_api_tree bi version file =
  Wiki_menu.build_tree_from_file bi
    ~create_service:(fun ?wiki file ->
      (version.Site_doc.api_service file :> Eliom_tools.get_page))
    ~file

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
  Lwt.return (Eliom_tools.Html5.hierarchical_menu_depth_first
                ~classe:["doctree"; "manualapitree"]
		(Eliom_tools.Not_clickable, manual_tree @ api_tree)
		~whole_tree:true
		?service
		())

let build_apitree ?service bi version =
  build_api_trees bi version >>= fun api_tree ->
  Lwt.return (Eliom_tools.Html5.hierarchical_menu_depth_first
                ~classe:["doctree"; "apitree"]
		(Eliom_tools.Not_clickable, api_tree)
		~whole_tree:true
		?service
		())

let build_manualtree ?service bi version =
  build_manual_tree bi version >>= fun manual_tree ->
  Lwt.return (Eliom_tools.Html5.hierarchical_menu_depth_first
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
    Eliom_output.Html5.make_string_uri ~absolute_path:true ~service () in
(*  assert(String.length prefix < String.length url);  *)
  assert(String.sub url 1 (String.length prefix) = prefix);
  String.sub url
    (String.length prefix + 2)
    (String.length url - String.length prefix - 2)

let append_version b bi level version title =
  let title =  if title = Site_doc.stable_version_name then "last stable" else title in
  let project = version.Site_doc.branch.Site_doc.br_project in
  let wiki_id = Wiki_types.string_of_wiki project.Site_doc.wiki in
  let manual = version.Site_doc.manual_service [""] in
  let api = version.Site_doc.api_service [""] in
  try
    ignore (version.Site_doc.api_resolver ["index"]);
    Format.bprintf b
      ("%s[[wiki(%s):%s|%s]]\n" ^^
       "=%s[[wiki(%s):%s|Manual]]\n" ^^
       "=%s[[wiki(%s):%s|API Reference]]\n")
      level wiki_id (get_relative_path_in_wiki bi project manual) title
      level wiki_id (get_relative_path_in_wiki bi project manual)
      level wiki_id (get_relative_path_in_wiki bi project api)
  with
  | _ ->
      Format.bprintf b
	"%s[[wiki(%s):%s|%s]]\n"
	level wiki_id (get_relative_path_in_wiki bi project manual) title

let build_version b bi level version =
  append_version b bi level version version.Site_doc.version

let build_versions b bi level ?last title versions =
  if versions <> [] || last <> None then
    let last_version =
      match last with
      | Some last -> last
      | None -> List.hd versions in
    let last_manual = last_version.Site_doc.manual_service [""] in
    let project = last_version.Site_doc.branch.Site_doc.br_project in
    let wiki_id = Wiki_types.string_of_wiki project.Site_doc.wiki in
    Format.bprintf b
      "%s[[wiki(%s):%s|%s]]\n"
      level wiki_id (get_relative_path_in_wiki bi project last_manual) title;
    (match last with
     | Some last -> append_version b bi (level^"=") last "darcs"
     | None -> ());
    List.iter (build_version b bi (level^"=")) versions

let build_branch b bi level branch =
  match branch.Site_doc.br_title with
  | None -> ()
  | Some title ->
      build_versions b bi level
	~last:branch.Site_doc.br_version
	title
	branch.Site_doc.br_versions

let build_doc b bi level project =
  let wiki_id = Wiki_types.string_of_wiki project.Site_doc.wiki in
  let last_stable_version = Site_doc.get_last_version project in
  let last_stable_manual = last_stable_version.Site_doc.manual_service [""] in
  Format.bprintf b
    "%s[[wiki(%s):%s|Doc]]\n"
    level wiki_id (get_relative_path_in_wiki bi project last_stable_manual);
  List.iter (build_branch b bi (level^"=")) project.Site_doc.branches;
  build_versions b bi (level^"=") "Older versions" project.Site_doc.old_versions

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
      [ HTML5.M.h1
	  [ HTML5.M.span ~a:[HTML5.M.a_class ["doclink_error"]] [HTML5.M.pcdata msg]] ] in

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

(* let build_version version : Wiki_menu.menu_item = *)
  (* let manual = version.Site_doc.manual_service [""] in *)
  (* let api = version.Site_doc.api_service "" in *)
  (* {{ {: version.Site_doc.version :} }}, *)
  (* Eliom_tools_common.Site_tree *)
    (* (Eliom_tools_common.Main_page manual, *)
     (* [{{ "Manual" }}, *)
      (* Eliom_tools_common.Site_tree (Eliom_tools_common.Main_page manual, []); *)
      (* {{ "API Reference" }}, *)
      (* Eliom_tools_common.Site_tree (Eliom_tools_common.Main_page api, []) ]) *)

(* let build_versions project : Wiki_menu.menu_item list = *)
  (* List.map build_version project.version *)


