
(** ... *)

let manual_wiki_dir = "/var/www/ocsigen_org/manualwiki"
let api_wiki_dir    = "/var/www/ocsigen_org/apiwiki"

(** Documentation des projets. *)

let eliom_id = 15
let server_id = 12
let lwt_id = 21
let js_of_ocaml_id = 30
let macaque_id = 25
let oclosure_id = 33
let ocsimore_id = 18
let obrowser_id = 9
let ocsforge_id = 24
let tyxml_id = 66

let projects = [

  (* Those datas should be in database (Ocsforge ??) *)

  (** Eliom *)

  eliom_id,
  [
   "dev"    , 298l, "services", ["1.91"], Some (2, "Development branch"),
      Some [["server"];["client"]];
   "1.3-dev", 298l, "intro"   , ["1.3.4";"1.3.3";"1.3.2";"1.3.1";"1.3.0"],
     Some (1, "Latest 1.3 (stable)"), None;
   "1.0-dev", 298l, "intro"   , ["1.1.0";"1.0.0"], None, None;
   "1.2-dev", 298l, "intro"   , ["1.2.2";"1.2.1";"1.2.0"], None, None;
  ],
  "1.3.4", (* last stable version *)
  303l;    (* wikibox unknown version *)

  (** Ocsigen server *)

  server_id,
  [
   "dev"    , 310l, "quickstart", ["1.91"], Some (2, "Development branch"), None;
   "1.3-dev", 310l, "intro"     , ["1.3.4";"1.3.3";"1.3.2";"1.3.1";"1.3.0"],
     Some (1, "Latest 1.3 (stable)"), None;
   "1.2-dev", 310l, "intro"     , ["1.2.2";"1.2.1";"1.2.0"], None, None;
   "1.0-dev", 310l, "intro"     , ["1.1.0";"1.0.0"], None, None;
  ],
  "1.3.4", (* last stable version *)
  311l; (* wikibox unknown version *)

  (** Lwt *)

  lwt_id,
  [
   "dev", 315l, "manual", ["2.3.0";"2.2.1";"2.2.0"], Some (1, "Stable branch"), None;
   "old", 315l, "manual", ["2.1.1";"2.1.0";"2.0.0";"1.1.0";"1.0.0"], None, None;
  ],
  "2.3.0", (* last stable version *)
  317l;    (* wikibox unknown version *)

  (** Js_of_ocaml *)

  js_of_ocaml_id,
  [
   "dev", 323l, "intro", ["1.0.2";"1.0.1";"1.0.0"], Some (1, "Stable branch"), None;
  ],
  "1.0.2", (* last stable version *)
  322l;

  (** O'Closure *)

  oclosure_id,
  [
  "dev", 328l, "intro", [], Some (1, "Development branch"), None;
  ],
  "dev", (* last stable version*)
  329l;

  (** Ocsimore *)

  ocsimore_id,
  [
  "dev", 332l, "intro", [], Some (1, "Development branch"), None;
  ],
  "dev", (* last stable version*)
  333l;

  (** O'Browser *)

  obrowser_id,
  [
  "dev", 207l, "intro", [], Some (1, "Development branch"), None;
  ],
  "dev", (* last stable version*)
  334l;

  (** TyXML *)

  tyxml_id,
  [
  "dev", 345l, "intro", [], Some (1, "Development branch"), None;
  ],
  "dev",
  346l;
]

(** 'Tutorial' est un cas spécial. *)

let tutorial_id = 36
let tutorial_wikibox = 295l
let tutorial_default = "intro"

let tutorial_version = ["1.91"; "2.0-rc1"; "dev"]
let tutorial_last = "2.0-rc1"



(** CSS *)

let site_css () = [
  Eliom_output.Html5.css_link
    ~a:[ HTML5.M.a_title "Default";
	 HTML5.M.a_media [`Screen]; ]
    ~uri:(Eliom_output.Html5.make_uri
	    ~service:(Eliom_services.static_dir ())
	    ["resources";"doc";"style.css"]) ();
  Eliom_output.Html5.css_link
    ~a:[ HTML5.M.a_title "Printable";
	 HTML5.M.a_rel [`Alternate];
	 HTML5.M.a_media [`Screen]; ]
    ~uri:(Eliom_output.Html5.make_uri
	    ~service:(Eliom_services.static_dir ())
	    ["resources";"doc";"printable.css"]) ();
  Eliom_output.Html5.css_link
    ~a:[ HTML5.M.a_title "Printable";
	 HTML5.M.a_media [`Print]; ]
    ~uri:(Eliom_output.Html5.make_uri
	    ~service:(Eliom_services.static_dir ())
	    ["resources";"doc";"printable.css"]) ();
  Eliom_output.Html5.css_link
    ~uri:(Eliom_output.Html5.make_uri
	    ~service:(Eliom_services.static_dir ())
	    ["resources";"doc";"codecolor.css"]) ();
]

let doc_css =
  Eliom_output.Html5.css_link
    ~a:[ HTML5.M.a_media [`Screen]; ]
    ~uri:(Page_site.static_file_uri ["css";"docstyle.css"])

(** Gestion des erreurs. *)

let (>>=) = Lwt.bind
let (>|=) m f = Lwt.map f m

exception Error of string

let error (msg:string) =
  Lwt.return [ HTML5.M.span ~a:[HTML5.M.a_class ["doclink_error"]] [HTML5.M.pcdata msg] ]

let wrap_phrasing name f = fun bi args contents ->
  Wikicreole.Phrasing_without_interactive
    (lwt content =
       try_lwt
	 f bi args contents
       with
      | Error msg -> error (Format.sprintf "Error %s: %s" name msg)
      | exc ->
         error (Format.sprintf "Error %s: exception %s" name
		  (Printexc.to_string exc) ) in
     Lwt.return [HTML5.M.span content])

let wrap_flow5 name f = fun bi args contents ->
  Wikicreole.Flow5
    (try_lwt
	f bi args contents
     with
       | Error msg -> error (Format.sprintf "Error %s: %s" name msg)
       | exc ->
	 error (Format.sprintf "Error %s: exception %s" name
		  (Printexc.to_string exc) ) )

(** Contexte des services et des extensions. *)

(* GRGR FIXME use eref ~scope:Request *)
let current_version : string Polytables.key = Polytables.make_key ()
let set_current_version version =
  let table = Eliom_request_info.get_request_cache () in
  Polytables.set ~table ~key:current_version ~value:version

let stable_version_name = "" (* internal version number for the last stable version *)

let guess_version () =
  try
    let table = Eliom_request_info.get_request_cache () in
    Polytables.get ~table ~key:current_version
  with
  | Not_found -> stable_version_name

type project = {
    wiki_id: Wiki_types.wiki;
    path: string;
    wiki_box_404: Wiki_types.wikibox;
    mutable versions: version list;
    mutable last_stable: version option;
    mutable branches: branch list;
    mutable old_versions: version list;
  }
and version = {
    version: string;
    branch: branch;
    manual_resolver: string list Wiki_dir.resolver;
    manual_service: string list -> Eliom_tools_common.get_page;
    manual_menu: unit -> Ocsigen_local_files.resolved;
    aux_resolver: string list Wiki_dir.resolver;
    aux_service: string list -> Eliom_tools_common.get_page;
    api_resolver: string list Wiki_dir.resolver;
    api_service: string list -> Eliom_tools_common.get_page;
    api_menu: unit -> Ocsigen_local_files.resolved list;
  }
and branch = {
    br_project: project;
    br_name: string;
    br_wiki_box: Wiki_types.wikibox;
    br_title: string option;
    br_order: int;
    br_subprojects: string list list;
    mutable br_version: version;
    mutable br_versions: version list;
  }

let known_project : project list ref = ref []

exception Project_not_found

let find_project wiki_id =
  try
    List.find (fun p -> p.wiki_id = wiki_id ) !known_project
  with
  | Not_found -> raise Project_not_found

let version_404 project =
  let rec v = {
    version = "404";
    branch = { br_wiki_box = project.wiki_box_404; br_title = Some "404";
	       br_order=0; br_name = "404"; br_version = v;
	       br_versions = []; br_project = project; br_subprojects = []; };
    manual_resolver = (fun _ -> raise Wiki_dir.Undefined);
    manual_service = (fun _ -> raise Wiki_dir.Undefined);
    manual_menu = (fun () -> raise Wiki_dir.Undefined);
    aux_resolver = (fun _ -> raise Wiki_dir.Undefined);
    aux_service = (fun _ -> raise Wiki_dir.Undefined);
    api_resolver = (fun _ -> raise Wiki_dir.Undefined);
    api_service = (fun _ -> raise Wiki_dir.Undefined);
    api_menu = (fun () -> raise Wiki_dir.Undefined);
  } in
  v

let get_last_version project = match project.last_stable with
| Some v -> v
| None -> List.hd project.versions

let find_version (wiki_id, version) =
  let project = find_project wiki_id in
  match version with
  | None -> get_last_version project
  | Some version ->
    try
      List.find (fun v -> v.version = version) project.versions
    with
    | Not_found -> version_404 project

let register_project ~wiki_id ~path ~wb404 =
  known_project := {
    wiki_id = wiki_id;
    wiki_box_404 = wb404;
    path = String.concat "/" path;
    versions = [];
    branches = [];
    old_versions = [];
    last_stable = None;
  } :: !known_project

let compare_version v1 v2 =
  match v2.version, v1.version with
    | "", "" -> 0
    | "", _ -> 1
    | _, "" -> -1
    | v2, v1 -> Pervasives.compare v2 v1
let compare_branch b1 b2 = Pervasives.compare b1.br_order b2.br_order
let rec insert c v = function
  | [] -> [v]
  | v' :: _ as vs when c v v' <= 0 -> v :: vs
  | v' :: vs -> v' :: insert c v vs
let insert_version = insert compare_version
let insert_branch = insert compare_branch

let register_branch ~wiki_id ~wiki_box
    ?(manual_resolver = fun _ -> raise Wiki_dir.Undefined)
    ?(manual_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_manual_service = fun _ -> raise Wiki_dir.Undefined)
    ?(manual_menu = fun () -> raise Wiki_dir.Undefined)
    ?(aux_resolver = fun _ -> raise Wiki_dir.Undefined)
    ?(aux_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_aux_service = fun _ -> raise Wiki_dir.Undefined)
    ?(api_resolver = fun _ _ -> raise Wiki_dir.Undefined)
    ?(api_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_api_service = fun _ -> raise Wiki_dir.Undefined)
    ?(api_menu = fun _ -> raise Wiki_dir.Undefined)
    ?title
    ?(versions = [])
    ?(subprojects = [[]])
    last_stable_version
    name =
  let order, title = match title with
    | None -> max_int, None
    | Some (o, t) -> o, Some t in
  let project = List.find (fun p -> p.wiki_id = wiki_id) !known_project in
  let rec branch = {
    br_project = project;
    br_wiki_box = wiki_box;
    br_name = name;
    br_order = order;
    br_title = title;
    br_version = version;
    br_versions = [];
    br_subprojects = subprojects;
  } and version = {
    version = name;
    branch = branch;
    manual_resolver = manual_resolver;
    manual_service = (manual_service name :> string list -> Eliom_tools_common.get_page);
    manual_menu = manual_menu;
    aux_resolver = aux_resolver;
    aux_service = (aux_service name :> string list -> Eliom_tools_common.get_page);
    api_resolver = api_resolver name;
    api_service = (api_service name :> string list -> Eliom_tools_common.get_page);
    api_menu = api_menu name;
  } in
  project.branches <- insert_branch branch project.branches;
  project.versions <- insert_version version project.versions;
  let add_version v =
    let v = {
      version = v;
      branch = branch;
      manual_resolver = manual_resolver;
      manual_service = (manual_service v :> string list -> Eliom_tools_common.get_page);
      manual_menu = manual_menu;
      aux_resolver = aux_resolver;
      aux_service = (aux_service v :> string list -> Eliom_tools_common.get_page);
      api_resolver = api_resolver v;
      api_service = (api_service v :> string list -> Eliom_tools_common.get_page);
      api_menu = api_menu v;
    } in
    branch.br_versions <- insert_version v branch.br_versions;
    project.versions <- insert_version v project.versions;
    if v.version = last_stable_version then begin
      let v = {
	version = stable_version_name;
	branch = branch;
	manual_resolver = manual_resolver;
	manual_service = (default_manual_service :> string list -> Eliom_tools_common.get_page);
	manual_menu = manual_menu;
	aux_resolver = aux_resolver;
	aux_service = (default_aux_service :> string list -> Eliom_tools_common.get_page);
	api_resolver = api_resolver last_stable_version;
	api_service = (default_api_service :> string list -> Eliom_tools_common.get_page);
	api_menu = api_menu last_stable_version;
      } in
      branch.br_versions <- insert_version v branch.br_versions;
      project.versions <- insert_version v project.versions;
      project.last_stable <- Some v;
    end;
    if title = None then
      project.old_versions <- insert_version v project.old_versions
  in
  List.iter add_version versions


(** Creation des services. *)

let get_wiki_path id =
  (Wiki_sql.get_wiki_info_by_id id) >|= fun info ->
  match info.Wiki_types.wiki_pages with
  | Some pages -> Neturl.split_path pages
  | _ -> assert false

let register_project_data (id, branches, last_stable, wb404_unknown_version) =
  let wiki_id = Wiki_types.wiki_of_string (string_of_int id) in
  get_wiki_path wiki_id >>= fun path ->

  register_project
      ~wiki_id ~path
      ~wb404:(Wiki_types.wikibox_of_sql wb404_unknown_version);

   let process_manual (version, ((), file)) =
     set_current_version version;
     let version = find_version (wiki_id, Some version) in
     Wiki_dir.process
       ~wiki_id
       ~wrapper:(Wiki_dir.make_wrapper_of_wikibox ~wb:version.branch.br_wiki_box)
       ~resolve_wiki_menu_file:version.manual_resolver
       ~resolve_wiki_file:version.manual_resolver
       ~css:(fun () -> doc_css () :: site_css ())
       () file in

  let manual_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
		     (Eliom_parameters.prod
			(Eliom_parameters.string "version")
			(Eliom_parameters.prod
			   (Eliom_parameters.suffix_const "manual")
			   (Eliom_parameters.all_suffix "file"))))
      process_manual in

  let default_manual_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
			(Eliom_parameters.prod
			   (Eliom_parameters.suffix_const "manual")
			   (Eliom_parameters.all_suffix "file")))
      (fun arg -> process_manual (stable_version_name, arg)) in

  let process_aux (version, ((), file)) =
    let version = find_version (wiki_id, Some version) in
    Wiki_dir.serve_file
      ~wiki_id
      ~resolve_wiki_menu_file:version.manual_resolver
      ~resolve_file:version.aux_resolver
      ~wrapper:(Wiki_dir.make_wrapper_of_wikibox ~wb:version.branch.br_wiki_box)
      ~css:(fun () -> doc_css () :: site_css ())
      ()
      file
  in

  let aux_service =
    Eliom_output.Any.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
		     (Eliom_parameters.prod
			(Eliom_parameters.string "version")
			(Eliom_parameters.prod
			   (Eliom_parameters.suffix_const "files")
			   (Eliom_parameters.all_suffix "file"))))
      process_aux in

  let default_aux_service =
    Eliom_output.Any.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
		     (Eliom_parameters.prod
			(Eliom_parameters.suffix_const "files")
			(Eliom_parameters.all_suffix "file")))
      (fun arg -> process_aux (stable_version_name, arg)) in

  let process_api (version, ((), file)) () =
	set_current_version version;
	let version = find_version (wiki_id, Some version) in
	Wiki_dir.process
	  ~wiki_id
	  ~wrapper:(Wiki_dir.make_wrapper_of_wikibox ~wb:version.branch.br_wiki_box)
	  ~resolve_wiki_menu_file:version.manual_resolver
	  ~resolve_wiki_file:version.api_resolver
	  ~css:(fun () -> doc_css () :: site_css ())
	  () file () in

  let api_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
		     (Eliom_parameters.prod
			(Eliom_parameters.string "version")
			(Eliom_parameters.prod
			   (Eliom_parameters.suffix_const "api")
			   (Eliom_parameters.all_suffix "file"))))
      process_api  in

  let default_api_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
			(Eliom_parameters.prod
			   (Eliom_parameters.suffix_const "api")
			   (Eliom_parameters.all_suffix "file")))
      (fun arg -> process_api (stable_version_name, arg))  in

  let _ =
    (* redirect <project>/<version>/ to <project>/<version>/manual/ *)
    Eliom_output.Redirection.register_service
      ~options:`Permanent
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
		     (Eliom_parameters.prod
			(Eliom_parameters.string "version")
			(Eliom_parameters.suffix_const "")))
      (fun (version,()) () ->
	Lwt.return (Eliom_services.preapply manual_service (version, ((), [""])))) in

  let manual_resolver project ?default branch =
    let manual_dir =
      String.concat "/" (manual_wiki_dir :: project @ [ branch ; "src" ]) in
    Wiki_dir.resolve_file_in_dir ?default ~suffix:".wiki" manual_dir

  and aux_resolver project branch =
    let aux_dir =
      String.concat "/" (manual_wiki_dir :: project @ [ branch ; "files" ]) in
    Wiki_dir.resolve_file_in_dir aux_dir

  and api_resolver project version file =
    let api_dir =
      String.concat "/" (api_wiki_dir :: project @ [ version ]) in
    let file = if file = [] then ["index"] else file in
    Wiki_dir.resolve_file_in_dir ~default:"index" ~suffix:".wiki" api_dir file in

  let register_branch wiki_id project
      (branch, wiki_box, default, versions, title, subprojects) =
    let subprojects = match subprojects with None -> [[]] | Some l -> l in
    let wiki_box = Wiki_types.wikibox_of_sql wiki_box in
    register_branch ~wiki_id ~wiki_box
      ~manual_resolver:(manual_resolver project ~default branch)
      ~manual_service:
        (fun version file ->
	  Eliom_services.preapply manual_service (version, ((),file)))
      ~default_manual_service:
        (fun file -> Eliom_services.preapply default_manual_service ((),file))
      ~manual_menu:(fun () -> manual_resolver project branch ["menu"])
      ~aux_resolver:(aux_resolver project branch)
      ~aux_service:
        (fun version file ->
	  Eliom_services.preapply aux_service (version, ((),file)))
      ~default_aux_service:
        (fun file -> Eliom_services.preapply default_aux_service ((),file))
      ~api_resolver:(fun version -> api_resolver project version)
      ~api_service:
	(fun version file ->
	  Eliom_services.preapply api_service (version, ((),file)))
      ~default_api_service:
        (fun file -> Eliom_services.preapply default_api_service ((),file))
      ~api_menu:(fun version () ->
	List.map
	  (fun subproject -> api_resolver project version (subproject @ ["menu"]))
	  subprojects)
      ~versions
      ~subprojects
      ?title
      last_stable
      branch in

  List.iter (register_branch wiki_id path) (List.rev branches);
  Lwt.return ()

let () =
  Lwt_unix.run
    (Lwt_list.iter_p register_project_data projects : unit Lwt.t)

(** Enregistrement du tutorial *)

let tutorial_wiki      = Wiki_types.wiki_of_string (string_of_int tutorial_id)
let tutorial_dir     v = manual_wiki_dir ^ "/tutorial/" ^ v ^ "/src"
let tutorial_aux_dir v = manual_wiki_dir ^ "/tutorial/" ^ v ^ "/files"

let tutorial_path =
  (Wiki_sql.get_wiki_info_by_id tutorial_wiki) >|= fun info ->
    match info.Wiki_types.wiki_pages with
    | Some pages -> Neturl.split_path pages
    | _ -> assert false

let tutorial_resolver v =
  Wiki_dir.resolve_file_in_dir ~default:tutorial_default ~suffix:".wiki" (tutorial_dir v)

let tutorial_aux_resolver v f =
  Wiki_dir.resolve_file_in_dir (tutorial_aux_dir v) f

let tutorial_service =
  tutorial_path >|= fun tutorial_path ->
  Ocsimore_appl.register_service
    ~path:tutorial_path
    ~priority:10
    ~get_params:
      (Eliom_parameters.suffix
                    (Eliom_parameters.prod
                       (Eliom_parameters.string "version")
                       (Eliom_parameters.all_suffix "file")))
    (fun (version, file) () ->
      set_current_version version;
      Wiki_dir.process
       ~wiki_id:tutorial_wiki
       ~wrapper:(Wiki_dir.make_wrapper_of_wikibox
		   ~wb:(Wiki_types.wikibox_of_sql tutorial_wikibox))
       ~resolve_wiki_menu_file:(tutorial_resolver version)
       ~resolve_wiki_file:(tutorial_resolver version)
       ~css:(fun () -> doc_css () :: site_css ())
       () file ())

let tutorial_default_service =
  tutorial_path >|= fun tutorial_path ->
  Ocsimore_appl.register_service
    ~path:tutorial_path
    ~priority:10
    ~get_params:
      (Eliom_parameters.suffix
         (Eliom_parameters.all_suffix "file"))
    (fun file () ->
      set_current_version stable_version_name;
      Wiki_dir.process
       ~wiki_id:tutorial_wiki
       ~wrapper:(Wiki_dir.make_wrapper_of_wikibox
                   ~wb:(Wiki_types.wikibox_of_sql tutorial_wikibox))
       ~resolve_wiki_menu_file:(tutorial_resolver tutorial_last)
       ~resolve_wiki_file:(tutorial_resolver tutorial_last)
       ~css:(fun () -> doc_css () :: site_css ())
       () file ())

let tutorial_aux_service =
  tutorial_path >|= fun tutorial_path ->
  let wrapper =
    Wiki_dir.make_wrapper_of_wikibox
      ~wb:(Wiki_types.wikibox_of_sql tutorial_wikibox) in
  Eliom_output.Any.register_service
    ~path:(tutorial_path @ ["files"])
    ~priority:10
    ~get_params:
      (Eliom_parameters.suffix
                    (Eliom_parameters.prod
                       (Eliom_parameters.string "version")
                       (Eliom_parameters.all_suffix "file")))
    (fun (version, file) () ->
      Wiki_dir.serve_file
        ~wiki_id:tutorial_wiki ~wrapper 
        ~resolve_file:(tutorial_aux_resolver version) () file ())

let tutorial_aux_default_service =
  tutorial_path >|= fun tutorial_path ->
  let wrapper =
    Wiki_dir.make_wrapper_of_wikibox
      ~wb:(Wiki_types.wikibox_of_sql tutorial_wikibox) in
  Eliom_output.Any.register_service
    ~path:(tutorial_path @ ["files"])
    ~priority:10
    ~get_params:
      (Eliom_parameters.suffix (Eliom_parameters.all_suffix "file"))
    (Wiki_dir.serve_file
       ~wiki_id:tutorial_wiki ~wrapper
       ~resolve_file:(tutorial_aux_resolver tutorial_last) ())

let () =
  Lwt_unix.run
    (lwt tutorial_path = tutorial_path in
     lwt tutorial_service = tutorial_service in
     lwt tutorial_default_service = tutorial_default_service in
     lwt tutorial_aux_service = tutorial_aux_service in
     lwt tutorial_aux_default_service = tutorial_aux_default_service in
     register_project
	~wiki_id:tutorial_wiki
	~path:tutorial_path
	~wb404:(Wiki_types.wikibox_of_sql 299l); (* unused *)
     List.iter
       (fun version ->
         register_branch
           ~wiki_id:tutorial_wiki
	   ~wiki_box:(Wiki_types.wikibox_of_sql tutorial_wikibox)
	   ~manual_resolver:(tutorial_resolver version)
	   ~manual_service:(fun version file ->
             Eliom_services.preapply tutorial_service (version, file))
	   ~default_manual_service:(fun file ->
             Eliom_services.preapply tutorial_default_service file)
	   ~manual_menu:(fun () -> tutorial_resolver version ["menu"])
	   ~aux_service:(fun version file ->
	     Eliom_services.preapply tutorial_aux_service (version, file))
	   ~default_aux_service:(fun file ->
	     Eliom_services.preapply tutorial_aux_default_service file)
	   ~aux_resolver:(tutorial_aux_resolver version)
           ~versions:[version]
	tutorial_last (* Last stable version *)
	version) (* default branch name *)
        tutorial_version;
     Lwt.return ())
