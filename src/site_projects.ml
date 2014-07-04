let wiki_dir = ref "/var/www/data/git/ocsigen.org-data"

(** Documentation des projets. *)

let rec last = function
  | [] -> assert false
  | [x] -> x
  | _::xs -> last xs

let is_directory l =
  let full = List.fold_left Filename.concat !wiki_dir l in
  let name = last (!wiki_dir :: l) in
  name.[0] <> '.' && name.[0] <> '_' && name.[0] <> '#' && Sys.is_directory full

let readdir l =
  let full = List.fold_left Filename.concat !wiki_dir l in
  let all = Array.to_list (Sys.readdir full) in
  List.filter (fun p -> is_directory (l@[p])) all

let readdir_full l =
  let full = List.fold_left Filename.concat !wiki_dir l in
  let all = Array.to_list (Sys.readdir full) in
  let config_file = Filename.concat full "config.js" in
  let config =
    if Sys.file_exists config_file
    then try
        Yojson.Safe.from_file config_file
      with _ -> Printf.eprintf "error while reading %s; ignore it..\n%!" config_file; `Assoc []
    else `Assoc [] in
  config, List.filter (fun p -> is_directory (l@[p])) all

let file_exists l =
  let full = List.fold_left Filename.concat !wiki_dir l in
  Sys.file_exists full

type t = {
  version : Version.t;
  name : string;
  wiki_id: int;
  wiki_id_404: int;
  wiki_id_403: int;
  manual_main: string;
  manual_template: string;
  manual_template_404: string;
  subdir : string list list;
  has_api : bool;
  has_manual : bool;
}
let default = {
  version = Version.Dev;

  wiki_id = -1;
  wiki_id_404 = -1;
  wiki_id_403 = -1;

  name = "";

  manual_template = "manualTemplate";
  manual_template_404 = "manualUnknownVersion";
  manual_main = "intro";

  subdir = [];
  has_api = false;
  has_manual = false;
}

let get_string name l f c =
  try
    let json = List.assoc name l in
    let l = List.remove_assoc name l in
    match json with
    | `String s -> l, f c s
    | _ ->
      Printf.eprintf "malformed field %s; ignore it\n%!" name;
      l,c
  with Not_found ->
    l,c

let get_int name l f c =
  try
    let json = List.assoc name l in
    let l = List.remove_assoc name l in
    match json with
    | `Int s -> l, f c s
    | _ ->
      Printf.eprintf "malformed field %s; ignore it\n%!" name;
      l,c
  with Not_found ->
    l,c

let get_list name l f c =
  try
    let json = List.assoc name l in
    let l = List.remove_assoc name l in
    match json with
    | `List j -> l, f c j
    | _ ->
      Printf.eprintf "malformed field %s; ignore it\n%!" name;
      l,c
  with Not_found ->
    l,c

let rec update ?version c (json : Yojson.Safe.json ) =
  match json with
  | `Assoc [] -> c
  | `Assoc l ->
    let l,c = get_int "wiki_id" l (fun c i -> {c with wiki_id = i}) c in
    let l,c = get_int "wiki_id_404" l (fun c i -> {c with wiki_id_404 = i}) c in
    let l,c = get_int "wiki_id_403" l (fun c i -> {c with wiki_id_403 = i}) c in
    let l,c = get_string "manual_main" l (fun c s -> {c with manual_main = s}) c in
    let l,c = get_string "manual_template" l (fun c s -> {c with manual_template = s}) c in
    let l,c = get_string "manual_template_404" l (fun c s -> {c with manual_template_404 = s}) c in
    let l,c = get_list "versions" l (fun c json ->
        match json,version with
        | _, None -> c
        | l, Some current_version ->
          List.fold_left (fun c x ->
              match x with
              | `Assoc ll ->
                begin
                  match get_string "version" ll (fun _ s -> Some s) None with
                  | ll, Some v when Version.match_ v current_version ->
                    (* Printf.printf "%s match %s\n%!" (Version.to_string current_version) v; *)
                    update ~version:current_version c (`Assoc ll)
                  | _ -> c
                end
              | _ -> c) c l
      ) c in
    assert(l=[]);
    c
  | _ ->
    Printf.eprintf "malformed json; ignore it\n%!";
    c

let read_projects () =
  let conf = default in
  let main_config,projects = readdir_full [] in
  let conf = update conf main_config in
  List.map (fun p ->
      let conf = {conf with name = p} in
      let p_config,_versions = readdir_full [p] in
      let conf = update conf p_config in
      conf) projects

let read_versions p conf =
  let p_config,versions = readdir_full [p] in
  let conf = update conf p_config in
  (* Printf.printf "num of version %d\n%!" (List.length versions); *)
  let versions = List.map (fun v ->
      let has_api = file_exists [p;v;"api"] in
      let has_manual = file_exists [p;v;"manual";"src"] in
      let sub = if has_api then readdir [p;v;"api"] else [] in
      let version = Version.parse v in
      let conf = update ~version conf p_config in
      let conf = {conf with
                  subdir = List.map (fun x -> [x]) sub;
                  has_api;
                  has_manual;
                  version} in
      if (has_api && has_manual)
      then Some conf
      else if not (has_api || has_manual)
      then
        begin
          Printf.printf "No documentation found for %s/%s\n%!" p (Version.to_string version);
          if version <> Version.Dev
          then (Printf.printf "ignoring %s/%s\n%!" p (Version.to_string version); None)
          else Some conf
        end
      else
        begin
          Printf.printf "Missing documentation for %s/%s api:%b manual:%b\n%!"
            p (Version.to_string version)
            has_api has_manual;
          Some conf
        end
    ) versions in
  let versions = List.map (function Some x -> x | _ -> assert false)
      (List.filter (function Some _ -> true | _ -> false) versions) in
  versions
