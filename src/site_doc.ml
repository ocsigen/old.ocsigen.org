(** ... *)

open Eliom_content

let () =
  Eliom_config.parse_config
    Ocsigen_extensions.Configuration.([
        element ~name:"data-wiki"
          ~attributes:[ attribute ~name:"dir" (fun dir -> Site_projects.wiki_dir := dir) ] ();
      ]);
  Printf.printf "DATA wiki directory: %s\n%!" !Site_projects.wiki_dir;
  ()

(** Gestion des erreurs. *)

exception Error of string

let error (msg:string) =
  Lwt.return [ Html5.F.span ~a:[Html5.F.a_class ["doclink_error"]] [Html5.F.pcdata msg] ]

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
     Lwt.return [Html5.F.span content])

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

let current_version = Eliom_reference.Volatile.eref ~scope:Eliom_common.request_scope None

let stable_version_name = "" (* internal version number for the last stable version *)

let guess_version () =
  match Eliom_reference.Volatile.get current_version with
  | Some v -> v
  | None -> stable_version_name
type wiki_service =
    (unit, unit, Eliom_service.get_service_kind, Eliom_service.attached, Eliom_service.service_kind, [ `WithoutSuffix ],
     unit, unit, Eliom_service.registrable,
     Eliom_registration.appl_service) Eliom_service.service

type project = {
    wiki: Wiki_types.wiki;
    path: string;
    template_404: string; (* version not found *)
    mutable versions: version list;
    mutable last_stable: version option;
    mutable dev_version: version option;
  }
and version = {
    version: Version.t;
    snippet: string;
    project: project;
    subdir: string list list;
    template: string;
    manual_resolver: string list Wiki_dir.resolver;
    manual_service: string list -> wiki_service;
    manual_menu: unit -> Ocsigen_local_files.resolved;
    files_resolver: string list Wiki_dir.resolver;
    files_service: string list -> wiki_service;
    api_resolver: string list Wiki_dir.resolver;
    api_service: string list -> wiki_service;
    api_menu: unit -> Ocsigen_local_files.resolved list;
}

let known_project : project list ref = ref []

exception Project_not_found

let find_project wiki =
  try
    List.find (fun p -> p.wiki = wiki ) !known_project
  with
  | Not_found -> raise Project_not_found

let version_404 project =
  let v = {
    version = Version.parse "404";
    snippet = "404";
    project;
    subdir = [];
    template = "";
    manual_resolver = (fun _ -> raise Wiki_dir.Undefined);
    manual_service = (fun _ -> raise Wiki_dir.Undefined);
    manual_menu = (fun _ -> raise Wiki_dir.Undefined);
    files_resolver = (fun _ -> raise Wiki_dir.Undefined);
    files_service = (fun _ -> raise Wiki_dir.Undefined);
    api_resolver = (fun _ -> raise Wiki_dir.Undefined);
    api_service = (fun _ -> raise Wiki_dir.Undefined);
    api_menu = (fun _ -> raise Wiki_dir.Undefined);
  } in
  v

let get_last_version project = match project.last_stable with
  | Some v -> v
  | None -> match project.versions with
    | x :: _ -> x
    | [] -> match project.dev_version with
      | Some x -> x
      | None -> raise Not_found

let find_version (wiki, version) =
  let project = find_project wiki in
  try
    match version with
    | None
    | Some "" -> get_last_version project
    | Some "dev" ->
      begin
        match project.dev_version with
        | None -> raise Not_found
        | Some v -> v
      end
    | Some version -> List.find (fun v -> v.snippet = version) project.versions
  with
  | Not_found -> version_404 project

let register_project ~wiki ~path ~template_404 =
  known_project := {
    wiki;
    template_404;
    path = String.concat "/" path;
    versions = [];
    last_stable = None;
    dev_version = None
  } :: !known_project

let rec insert c v = function
  | [] -> [v]
  | v' :: _ as vs when c v v' <= 0 -> v :: vs
  | v' :: vs -> v' :: insert c v vs
let insert_version = insert (fun y x -> Version.compare x.version y.version)

let register_version ~wiki ~template
    ?(manual_resolver = fun _ -> raise Wiki_dir.Undefined)
    ?(manual_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_manual_service = fun _ -> raise Wiki_dir.Undefined)
    ?(manual_menu = fun _ () -> raise Wiki_dir.Undefined)
    ?(files_resolver = fun _ -> raise Wiki_dir.Undefined)
    ?(files_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_files_service = fun _ -> raise Wiki_dir.Undefined)
    ?(api_resolver = fun _ _ -> raise Wiki_dir.Undefined)
    ?(api_service = fun _ _ -> raise Wiki_dir.Undefined)
    ?(default_api_service = fun _ -> raise Wiki_dir.Undefined)
    ?(api_menu = fun _ _ -> raise Wiki_dir.Undefined)
    ?(subprojects = [[]])
    version =
  let project = List.find (fun p -> p.wiki = wiki) !known_project in
  let name = Version.to_string version in
  let version = {
    version;
    snippet = name;
    project = project;
    subdir = subprojects;
    template;
    manual_resolver = manual_resolver name;
    manual_service = (manual_service name :> string list -> wiki_service);
    manual_menu = manual_menu name;
    files_resolver = files_resolver name;
    files_service = (files_service name :> string list -> wiki_service);
    api_resolver = api_resolver name;
    api_service = (api_service name :> string list -> wiki_service);
    api_menu = api_menu name;
  } in
  match version.version with
  | Version.Dev ->
    project.dev_version <- Some version
  | v ->
    project.versions <- insert_version version project.versions;
    match project.last_stable with
    | None -> project.last_stable <- Some version
    | Some c when Version.compare v c.version > 0 -> project.last_stable <- Some version
    | c -> ()

(** Creation des services. *)

let get_wiki_path id =
  lwt info = Wiki_sql.get_wiki_info_by_id id in
  match info.Wiki_types.wiki_pages with
  | Some pages -> Lwt.return (Neturl.split_path pages)
  | _ -> assert false

exception Undefined
exception Dir



let register_project_data p =

  Printf.printf "registering project %s (wiki_id:%d)\n%!" p.Site_projects.name p.Site_projects.wiki_id;

  let wiki = Wiki_types.wiki_of_string (string_of_int p.Site_projects.wiki_id) in
  lwt path = get_wiki_path wiki in

  register_project ~wiki ~path ~template_404:p.Site_projects.manual_template_404;
  let project = find_project wiki in

  let wb404 = Wiki_types.wikibox_of_sql (Int32.of_int p.Site_projects.wiki_id_404) in
  let wb403 = Wiki_types.wikibox_of_sql (Int32.of_int p.Site_projects.wiki_id_403) in

  let process_manual (version, ((), file)) () =
    Eliom_reference.Volatile.set current_version (Some version);
    let version = find_version (wiki, Some version) in
    lwt () = Wiki_menu.set_menu_resolver version.manual_resolver in
    Wiki_dir.process_wikifile
      ~wiki ~template:version.template
      ~wb404 ~wb403
      version.manual_resolver file in

  let process_files (version, ((), file)) () =
    Eliom_reference.Volatile.set current_version (Some version);
    let version = find_version (wiki, Some version) in
    lwt () = Wiki_menu.set_menu_resolver version.manual_resolver in
    Wiki_dir.process_auxfile
      ~options:2678400
      ~wiki ~template:version.template
      ~wb404 ~wb403
      version.files_resolver file in

  let process_api (version, ((), file)) () =
    Eliom_reference.Volatile.set current_version (Some version);
    let version = find_version (wiki, Some version) in
    lwt () = Wiki_menu.set_menu_resolver version.manual_resolver in
    Wiki_dir.process_wikifile
      ~wiki ~template:version.template
      ~wb404 ~wb403
      version.api_resolver file in

  let manual_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameter.suffix
                     (Eliom_parameter.prod
                        (Eliom_parameter.string "version")
                        (Eliom_parameter.prod
                           (Eliom_parameter.suffix_const "manual")
                           (Eliom_parameter.all_suffix "file"))))
      process_manual in

  let default_manual_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameter.suffix
                     (Eliom_parameter.prod
                        (Eliom_parameter.suffix_const "manual")
                        (Eliom_parameter.all_suffix "file")))
      (fun arg -> process_manual (stable_version_name, arg)) in

  let files_service =
    Eliom_registration.Any.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameter.suffix
                     (Eliom_parameter.prod
                        (Eliom_parameter.string "version")
                        (Eliom_parameter.prod
                           (Eliom_parameter.suffix_const "files")
                           (Eliom_parameter.all_suffix "file"))))
      process_files in

  let default_files_service =
    Eliom_registration.Any.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameter.suffix
                     (Eliom_parameter.prod
                        (Eliom_parameter.suffix_const "files")
                        (Eliom_parameter.all_suffix "file")))
      (fun arg -> process_files (stable_version_name, arg)) in

  let api_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameter.suffix
                     (Eliom_parameter.prod
                        (Eliom_parameter.string "version")
                        (Eliom_parameter.prod
                           (Eliom_parameter.suffix_const "api")
                           (Eliom_parameter.all_suffix "file"))))
      process_api  in

  let default_api_service =
    Ocsimore_appl.register_service
      ~path
      ~priority:10
      ~get_params:(Eliom_parameter.suffix
                        (Eliom_parameter.prod
                           (Eliom_parameter.suffix_const "api")
                           (Eliom_parameter.all_suffix "file")))
      (fun arg -> process_api (stable_version_name, arg))  in

  let _ =
    (* redirect <project>/<version>/ to <project>/<version>/manual/ *)
    Eliom_registration.Redirection.register_service
      ~options:`MovedPermanently
      ~path
      ~priority:10
      ~get_params:(Eliom_parameter.suffix
                     (Eliom_parameter.prod
                        (Eliom_parameter.string "version")
                        (Eliom_parameter.suffix_const "")))
      (fun (version,()) () ->
         let version' = Version.parse version in
         let versions = project.versions in
         if version' = Version.Dev || List.exists (fun v ->
             Version.compare v.version version' = 0) versions then
           Lwt.return (Eliom_service.preapply manual_service (version, ((), [""])))
         else
           Lwt.fail Eliom_common.Eliom_404) in

  let manual_resolver project ?default branch file =
    (* Printf.printf "resolve %s - %s - %s (default: %s)\n%!" (String.concat "_" project) branch (String.concat "/" file) *)
    (*   (match default with Some d -> d | None -> "n/a"); *)
    let file = match default,file with
      | Some d, ([]|[""]) -> [d]
      |  _ -> file in
    let manual_dir =
      String.concat "/" (!Site_projects.wiki_dir :: project @ [ branch ; "manual" ]) in
    Wiki_dir.resolve_file_in_dir ?default ~suffix:".wiki" manual_dir ("src" :: file) ()

  and files_resolver project branch file =
    let files_dir =
      String.concat "/" (!Site_projects.wiki_dir :: project @ [ branch ;  "manual"]) in
    Wiki_dir.resolve_file_in_dir files_dir ("files" :: file) ()

  and api_resolver project version file =
    let api_dir =
      String.concat "/" (!Site_projects.wiki_dir :: project @ [ version ; "api"]) in
    let file = match file with [] | [""] -> ["index"] | l -> file in
    Wiki_dir.resolve_file_in_dir ~default:"index" ~suffix:".wiki" api_dir file () in

  let register_version wiki project conf =
    let subprojects = match conf.Site_projects.subdir with [] -> [[]] | l -> l in
    register_version ~wiki ~template:conf.Site_projects.manual_template
      ~manual_resolver:(fun branch -> manual_resolver project ~default:conf.Site_projects.manual_main branch)
      ~manual_service:
        (fun version file ->
          Eliom_service.preapply manual_service (version, ((),file)))
      ~default_manual_service:
        (fun file -> Eliom_service.preapply default_manual_service ((),file))
      ~manual_menu:(fun branch () -> manual_resolver project branch ["menu"])
      ~files_resolver:(fun branch -> files_resolver project branch)
      ~files_service:
        (fun version file ->
          Eliom_service.preapply files_service (version, ((),file)))
      ~default_files_service:
        (fun file -> Eliom_service.preapply default_files_service ((),file))
      ~api_resolver:(fun version -> api_resolver project version)
      ~api_service:
        (fun version file ->
          Eliom_service.preapply api_service (version, ((),file)))
      ~default_api_service:
        (fun file -> Eliom_service.preapply default_api_service ((),file))
      ~api_menu:(fun version () ->
        List.map
          (fun subproject -> api_resolver project version (subproject @ ["menu"]))
          subprojects)
      ~subprojects
      conf.Site_projects.version in

  Lwt.return (
    p.Site_projects.name,
    (fun () ->
       project.last_stable <- None;
       project.dev_version <- None;
       project.versions <- [];
       let versions = Site_projects.read_versions p.Site_projects.name p in
       List.iter (register_version wiki path) versions))

let projects = Site_projects.read_projects ()

let reload_list : (string * (unit -> unit)) list =
  Lwt_unix.run
    (Lwt_list.map_p register_project_data projects)

let reload () =
  List.iter (fun (name,f) ->
      Printf.printf "loading updating versions for %s\n%!" name;
      f ()) reload_list

let _ =
  Ocsigen_extensions.register_command_function ~prefix:"ocsigen.org"
    (fun _ l ->
       match l with
       | ["read";"versions"] ->
         reload ();
         Lwt.return ()
       | _ -> Lwt.return ());
  reload ()
