
(** ... *)

let manual_wiki_dir = "/var/www/data/manualwiki"
let api_wiki_dir    = "/var/www/data/apiwiki"

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
let macaque_id = 25

let projects = [

  (* Those datas should be in database (Ocsforge ??) *)

  (** Eliom *)

(* (branch name,
    template page,
    default page,
    version list,
    long branch name (only if we want a special entry in the menu) 
      and order of appearance in menu,
    subdirectories for API doc (optional, for example Eliom client API and server API))
*)

  eliom_id,
  [
   "dev"    , "manualTemplate", "intro", [], Some (2, "Dev"),
      Some [["server"];["client"]];
   "2.0-dev", "manualTemplate", "intro", ["2.0.2";"2.0.1";"2.0"], Some (1, "Stable branch"),
      Some [["server"];["client"]];
   "1.3-dev", "manualTemplate", "intro"   , ["1.3.4";"1.3.3";"1.3.2";"1.3.1";"1.3.0"], None, None;
   "1.0-dev", "manualTemplate", "intro"   , ["1.1.0";"1.0.0"], None, None;
   "1.2-dev", "manualTemplate", "intro"   , ["1.2.2";"1.2.1";"1.2.0"], None, None;
  ],
  "2.0.2", (* last stable version *)
  "manualUnknownVersion",
  303l, (* 404 wiki box *)
  303l; (* 403 wiki box FIXME *)

  (** Ocsigen server *)

  server_id,
  [
   "dev"    , "manualTemplate", "quickstart", ["2.0.2";"2.0.1";"2.0"], Some (1, "Stable branch"), None;
   "1.3-dev", "manualTemplate", "intro"     , ["1.3.4";"1.3.3";"1.3.2";"1.3.1";"1.3.0"],
     None, None;
   "1.2-dev", "manualTemplate", "intro"     , ["1.2.2";"1.2.1";"1.2.0"], None, None;
   "1.0-dev", "manualTemplate", "intro"     , ["1.1.0";"1.0.0"], None, None;
  ],
  "2.0.2", (* last stable version *)
  "manualUnknownVersion",
  311l, (* 404 wiki box *)
  311l; (* 403 wiki box FIXME *)

  (** Lwt *)

  lwt_id,
  [
   "dev", "manualTemplate", "manual", ["2.3.2";"2.3.1";"2.3.0";"2.2.1";"2.2.0"], Some (1, "Stable branch"), None;
   "old", "manualTemplate", "manual", ["2.1.1";"2.1.0";"2.0.0";"1.1.0";"1.0.0"], None, None;
  ],
  "2.3.2", (* last stable version *)
  "manualUnknownVersion",
  317l, (* 404 wiki box *)
  317l; (* 403 wiki box FIXME *)

  (** Js_of_ocaml *)

  js_of_ocaml_id,
  [
   "dev", "manualTemplate", "overview",
   ["1.0.9"], Some (1,
   "Stable branch"), None;
   "old", "manualTemplate", "overview",
   ["1.0.8";"1.0.6";"1.0.5";"1.0.4";"1.0.3";"1.0.2";"1.0.1";"1.0.0"], None , None;
  ],
  "1.0.9", (* last stable version *)
  "manualUnknownVersion",
  322l, (* 404 wiki box *)
  322l; (* 403 wiki box FIXME *)

  (** O'Closure *)

  oclosure_id,
  [
  "dev", "manualTemplate", "intro", [], Some (1, "Development branch"), None;
  ],
  "dev", (* last stable version*)
  "manualUnknownVersion",
  329l, (* 404 wiki box *)
  329l; (* 403 wiki box FIXME *)

  (** Ocsimore *)

  ocsimore_id,
  [
  "dev", "manualTemplate", "intro", [], Some (1, "Development branch"), None;
  ],
  "dev", (* last stable version*)
  "manualUnknownVersion",
  333l, (* 404 wiki box *)
  333l; (* 403 wiki box FIXME *)

  (** O'Browser *)

  obrowser_id,
  [
  "dev", "manualTemplate", "intro", [], Some (1, "Development branch"), None;
  ],
  "dev", (* last stable version*)
  "manualUnknownVersion",
  334l, (* 404 wiki box *)
  334l; (* 403 wiki box FIXME *)

  (** TyXML *)

  tyxml_id,
  [
  "dev", "manualTemplate", "intro", ["2.0.1";"2.0"], Some (1, "Stable branch"), None;
  ],
  "2.0.1",
  "manualUnknownVersion",
  346l, (* 404 wiki box *)
  346l; (* 403 wiki box FIXME *)

  (** Macaque *)

  macaque_id,
  [
  "dev", "manualTemplate", "intro", [], Some (1, "Development branch"), None;
  ],
  "dev", (* last stable version *)
  "manualUnknownVersion",
  346l, (* 404 wiki box *)
  346l; (* 403 wiki box FIXME *)

]

(** 'Tutorial' est un cas spécial. *)

let tutorial_id = 36
let tutorial_template = "tutorialTemplate"
let tutorial_default = "intro"

let tutorial_version = ["2.0"; "dev"]
let tutorial_last = "2.0"

let tutorial_wb404 = Wiki_types.wikibox_of_sql 295l
let tutorial_wb403 = Wiki_types.wikibox_of_sql 295l

(** Gestion des erreurs. *)

exception Error of string

let error (msg:string) =
  Lwt.return [ HTML5.M.span ~a:[HTML5.M.a_class ["doclink_error"]] [HTML5.M.pcdata msg] ]

let wrap_phrasing name f = fun bi args contents ->
  `Phrasing_without_interactive
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
  `Flow5
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

type wiki_service =
    (unit, unit, Eliom_services.get_service_kind, [ `WithoutSuffix ],
     unit, unit, Eliom_services.registrable,
     Eliom_output.appl_service) Eliom_services.service

type project = {
    wiki: Wiki_types.wiki;
    path: string;
    template_404: string; (* version not found *)
    mutable versions: version list;
    mutable last_stable: version option;
    mutable branches: branch list;
    mutable old_versions: version list;
  }
and version = {
    version: string;
    branch: branch;
    manual_resolver: string list Wiki_dir.resolver;
    manual_service: string list -> wiki_service;
    manual_menu: unit -> Ocsigen_local_files.resolved;
    aux_resolver: string list Wiki_dir.resolver;
    aux_service: string list -> wiki_service;
    api_resolver: string list Wiki_dir.resolver;
    api_service: string list -> wiki_service;
    api_menu: unit -> Ocsigen_local_files.resolved list;
  }
and branch = {
    br_project: project;
    br_name: string;
    br_template: string;
    br_title: string option;
    br_order: int;
    br_subprojects: string list list;
    mutable br_version: version;
    mutable br_versions: version list;
  }

let known_project : project list ref = ref []

exception Project_not_found

let find_project wiki =
  try
    List.find (fun p -> p.wiki = wiki ) !known_project
  with
  | Not_found -> raise Project_not_found

let version_404 project =
  let rec v = {
    version = "404";
    branch = { br_template = project.template_404; br_title = Some "404";
	       br_order=0; br_name = "404"; br_version = v;
	       br_versions = []; br_project = project; br_subprojects = []; };
    manual_resolver = (fun _ -> raise Wiki_dir.Undefined);
    manual_service = (fun _ -> raise Wiki_dir.Undefined);
    manual_menu = (fun _ -> raise Wiki_dir.Undefined);
    aux_resolver = (fun _ -> raise Wiki_dir.Undefined);
    aux_service = (fun _ -> raise Wiki_dir.Undefined);
    api_resolver = (fun _ -> raise Wiki_dir.Undefined);
    api_service = (fun _ -> raise Wiki_dir.Undefined);
    api_menu = (fun _ -> raise Wiki_dir.Undefined);
  } in
  v

let get_last_version project = match project.last_stable with
| Some v -> v
| None -> List.hd project.versions

let find_version (wiki, version) =
  let project = find_project wiki in
  match version with
  | None -> get_last_version project
  | Some version ->
    try
      List.find (fun v -> v.version = version) project.versions
    with
    | Not_found -> version_404 project

let register_project ~wiki ~path ~template_404 =
  known_project := {
    wiki;
    template_404;
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
    | v1, v2 when v2 = v1 ^ "-src" -> 1
    | v2, v1 when v2 = v1 ^ "-src" -> -1
    | v2, v1 -> Pervasives.compare v2 v1
let compare_branch b1 b2 = Pervasives.compare b1.br_order b2.br_order
let rec insert c v = function
  | [] -> [v]
  | v' :: _ as vs when c v v' <= 0 -> v :: vs
  | v' :: vs -> v' :: insert c v vs
let insert_version = insert compare_version
let insert_branch = insert compare_branch

let register_branch ~wiki ~template
    ?(manual_resolver = fun _ -> raise Wiki_dir.Undefined)
    ?(manual_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_manual_service = fun _ -> raise Wiki_dir.Undefined)
    ?(manual_menu = fun _ () -> raise Wiki_dir.Undefined)
    ?(aux_resolver = fun _ -> raise Wiki_dir.Undefined)
    ?(aux_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_aux_service = fun _ -> raise Wiki_dir.Undefined)
    ?(api_resolver = fun _ _ -> raise Wiki_dir.Undefined)
    ?(api_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_api_service = fun _ -> raise Wiki_dir.Undefined)
    ?(api_menu = fun _ _ -> raise Wiki_dir.Undefined)
    ?title
    ?(versions = [])
    ?(subprojects = [[]])
    last_stable_version
    name =
  let order, title = match title with
    | None -> max_int, None
    | Some (o, t) -> o, Some t in
  let project = List.find (fun p -> p.wiki = wiki) !known_project in
  let src_name = name ^ "-src" in
  let rec branch = {
    br_project = project;
    br_template = template;
    br_name = name;
    br_order = order;
    br_title = title;
    br_version = version;
    br_versions = [];
    br_subprojects = subprojects;
  } and version = {
    version = name;
    branch = branch;
    manual_resolver = manual_resolver name;
    manual_service = (manual_service name :> string list -> wiki_service);
    manual_menu = manual_menu name;
    aux_resolver = aux_resolver name;
    aux_service = (aux_service name :> string list -> wiki_service);
    api_resolver = api_resolver name;
    api_service = (api_service name :> string list -> wiki_service);
    api_menu = api_menu name;
  } and src_version = {
    version = src_name;
    branch = branch;
    manual_resolver = manual_resolver src_name;
    manual_service = (manual_service src_name :> string list -> wiki_service);
    manual_menu = manual_menu src_name;
    aux_resolver = aux_resolver src_name;
    aux_service = (aux_service src_name :> string list -> wiki_service);
    api_resolver = api_resolver src_name;
    api_service = (api_service src_name :> string list -> wiki_service);
    api_menu = api_menu src_name;
  }in
  project.branches <- insert_branch branch project.branches;
  project.versions <- insert_version version project.versions;
  project.versions <- insert_version src_version project.versions;
  let add_version v =
    let v = {
      version = v;
      branch = branch;
      manual_resolver = manual_resolver name;
      manual_service = (manual_service v :> string list -> wiki_service);
      manual_menu = manual_menu name;
      aux_resolver = aux_resolver name;
      aux_service = (aux_service v :> string list -> wiki_service);
      api_resolver = api_resolver v;
      api_service = (api_service v :> string list -> wiki_service);
      api_menu = api_menu v;
    } in
    branch.br_versions <- insert_version v branch.br_versions;
    project.versions <- insert_version v project.versions;
    if v.version = last_stable_version then begin
      let v = {
	version = stable_version_name;
	branch = branch;
	manual_resolver = manual_resolver name;
	manual_service = (default_manual_service :> string list -> wiki_service);
	manual_menu = manual_menu name;
	aux_resolver = aux_resolver name;
	aux_service = (default_aux_service :> string list -> wiki_service);
	api_resolver = api_resolver last_stable_version;
	api_service = (default_api_service :> string list -> wiki_service);
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
  lwt info = Wiki_sql.get_wiki_info_by_id id in
  match info.Wiki_types.wiki_pages with
  | Some pages -> Lwt.return (Neturl.split_path pages)
  | _ -> assert false

exception Undefined
exception Dir

let register_project_data (id, branches, last_stable, template_404, wb404, wb403) =
  let wiki = Wiki_types.wiki_of_string (string_of_int id) in
  lwt path = get_wiki_path wiki in

  register_project ~wiki ~path ~template_404;

  let wb404 = Wiki_types.wikibox_of_sql wb404 in
  let wb403 = Wiki_types.wikibox_of_sql wb403 in

  let process_manual (version, ((), file)) () =
    set_current_version version;
    let version = find_version (wiki, Some version) in
    lwt () = Wiki_menu.set_menu_resolver version.manual_resolver in
    Wiki_dir.process_wikifile
      ~wiki ~template:version.branch.br_template
      ~wb404 ~wb403
      version.manual_resolver file in

  let process_aux (version, ((), file)) () =
    set_current_version version;
    let version = find_version (wiki, Some version) in
    lwt () = Wiki_menu.set_menu_resolver version.manual_resolver in
    Wiki_dir.process_auxfile
      ~options:2678400
      ~wiki ~template:version.branch.br_template
      ~wb404 ~wb403
      version.aux_resolver file in

  let process_api (version, ((), file)) () =
    set_current_version version;
    let version = find_version (wiki, Some version) in
    lwt () = Wiki_menu.set_menu_resolver version.manual_resolver in
    Wiki_dir.process_wikifile
      ~wiki ~template:version.branch.br_template
      ~wb404 ~wb403
      version.api_resolver file in

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
      ~options:`MovedPermanently
      ~path
      ~priority:10
      ~get_params:(Eliom_parameters.suffix
		     (Eliom_parameters.prod
			(Eliom_parameters.string "version")
			(Eliom_parameters.suffix_const "")))
      (fun (version,()) () ->
	if List.exists (fun (name, _, _, versions, _, _) -> name = version || List.exists ((=) version) versions) branches then
	  Lwt.return (Eliom_services.preapply manual_service (version, ((), [""])))
	else
	  Lwt.fail Eliom_common.Eliom_404) in

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

  let register_branch wiki project
      (branch, template, default, versions, title, subprojects) =
    let subprojects = match subprojects with None -> [[]] | Some l -> l in
    register_branch ~wiki ~template
      ~manual_resolver:(fun branch -> manual_resolver project ~default branch)
      ~manual_service:
        (fun version file ->
	  Eliom_services.preapply manual_service (version, ((),file)))
      ~default_manual_service:
        (fun file -> Eliom_services.preapply default_manual_service ((),file))
      ~manual_menu:(fun branch () -> manual_resolver project branch ["menu"])
      ~aux_resolver:(fun branch -> aux_resolver project branch)
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

  List.iter (register_branch wiki path) (List.rev branches);
  Lwt.return ()

let () =
  Lwt_unix.run
    (Lwt_list.iter_p register_project_data projects : unit Lwt.t)

(** Enregistrement du tutorial *)

let tutorial_wiki      = Wiki_types.wiki_of_string (string_of_int tutorial_id)
let tutorial_dir     v = manual_wiki_dir ^ "/tutorial/" ^ v ^ "/src"
let tutorial_aux_dir v = manual_wiki_dir ^ "/tutorial/" ^ v ^ "/files"

let tutorial_path =
  lwt info = Wiki_sql.get_wiki_info_by_id tutorial_wiki in
  match info.Wiki_types.wiki_pages with
  | Some pages -> Lwt.return (Neturl.split_path pages)
  | _ -> assert false

let tutorial_path = Lwt_unix.run tutorial_path

let tutorial_resolver v =
  Wiki_dir.resolve_file_in_dir ~default:tutorial_default ~suffix:".wiki" (tutorial_dir v)

let tutorial_aux_resolver v f =
  Wiki_dir.resolve_file_in_dir (tutorial_aux_dir v) f

let tutorial_service =
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
      lwt () = Wiki_menu.set_menu_resolver (tutorial_resolver version) in
      Wiki_dir.process_wikifile
	~wiki:tutorial_wiki ~template:tutorial_template
	~wb404:tutorial_wb404 ~wb403:tutorial_wb403
	(tutorial_resolver version) file)

let tutorial_default_service =
  Ocsimore_appl.register_service
    ~path:tutorial_path
    ~priority:10
    ~get_params:
    (Eliom_parameters.suffix
       (Eliom_parameters.all_suffix "file"))
    (fun file () ->
      set_current_version stable_version_name;
      lwt () = Wiki_menu.set_menu_resolver (tutorial_resolver tutorial_last) in
      Wiki_dir.process_wikifile
	~wiki:tutorial_wiki ~template:tutorial_template
	~wb404:tutorial_wb404 ~wb403:tutorial_wb403
	(tutorial_resolver tutorial_last) file)

let tutorial_aux_service =
  Eliom_output.Any.register_service
    ~path:(tutorial_path)
    ~priority:10
    ~get_params:
    (Eliom_parameters.suffix
       (Eliom_parameters.prod
          (Eliom_parameters.string "version")
	  (Eliom_parameters.prod
	     (Eliom_parameters.suffix_const "files")
             (Eliom_parameters.all_suffix "file"))))
    (fun (version, ((), file)) () ->
      set_current_version version;
      lwt () = Wiki_menu.set_menu_resolver (tutorial_resolver version) in
      Wiki_dir.process_auxfile
	~options:2678400
	~wiki:tutorial_wiki ~template:tutorial_template
	~wb404:tutorial_wb404 ~wb403:tutorial_wb403
	(tutorial_aux_resolver version) file)

let tutorial_aux_default_service =
  Eliom_output.Any.register_service
    ~path:(tutorial_path @ ["files"])
    ~priority:10
    ~get_params:
    (Eliom_parameters.suffix (Eliom_parameters.all_suffix "file"))
    (fun file () ->
      set_current_version stable_version_name;
      lwt () = Wiki_menu.set_menu_resolver (tutorial_resolver tutorial_last) in
      Wiki_dir.process_auxfile
	~options:2678400
	~wiki:tutorial_wiki ~template:tutorial_template
	~wb404:tutorial_wb404 ~wb403:tutorial_wb403
	(tutorial_aux_resolver tutorial_last) file)

let () =
  register_project
    ~wiki:tutorial_wiki
    ~path:tutorial_path
    ~template_404:tutorial_template;
  List.iter
    (fun version ->
      register_branch
        ~wiki:tutorial_wiki
	~template:tutorial_template
	~manual_resolver:tutorial_resolver
	~manual_service:(fun version file ->
          Eliom_services.preapply tutorial_service (version, file))
	~default_manual_service:(fun file ->
          Eliom_services.preapply tutorial_default_service file)
	~manual_menu:(fun version () -> tutorial_resolver version ["menu"])
	~aux_service:(fun version file ->
	  Eliom_services.preapply tutorial_aux_service (version, ((), file)))
	~default_aux_service:(fun file ->
	  Eliom_services.preapply tutorial_aux_default_service file)
	~aux_resolver:tutorial_aux_resolver
        ~versions:[version]
	tutorial_last (* Last stable version *)
	version) (* default branch name *)
    tutorial_version
