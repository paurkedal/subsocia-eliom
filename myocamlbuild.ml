open Ocamlbuild_plugin

module M = Ocamlbuild_eliom.Make (struct
  let client_dir = "client"
  let server_dir = "server"
  let type_dir = "type"
end)

let oasis_executables = [
  "web/client/sociaweb_app.byte";
]

let pkg_datadir pkgname =
  let libdir = Findlib.((query pkgname).location) in
  let librootdir = Filename.dirname libdir in
  let prefix = Filename.dirname librootdir in
  let datarootdir = Filename.concat prefix "share" in
  Filename.concat datarootdir pkgname

let panograph_datadir = pkg_datadir "panograph"

let sed_rule ~dep ~prod scripts =
  rule (dep ^ " -> " ^ prod) ~dep ~prod
    (fun env build ->
      let dep = env dep and prod = env prod in
      let script_args = List.map (fun script -> S[A"-e"; A script]) scripts in
      Cmd (S[A"sed"; S script_args; P dep; Sh">"; Px prod]))

let local_rules () =
  sed_rule ~dep:"ocsigen-dev.conf.in" ~prod:"ocsigen-dev.conf"
    ["s;@PANOGRAPH_DATADIR@;" ^ pkg_datadir "panograph" ^ ";g"]

let js_of_ocaml_version =
  let ic = Unix.open_process_in "js_of_ocaml --version" in
  let version = input_line ic in
  (match Unix.close_process_in ic with
   | Unix.WEXITED 0 -> ()
   | _ -> failwith "js_of_ocaml --version failed");
  (match String.split_on_char '.' (String.trim version) with
   | [] | [_] -> failwith "Failed to parse js_of_ocaml version."
   | v0 :: v1 :: _ -> (int_of_string v0, int_of_string v1))

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  (match hook with
   | Before_options -> Options.make_links := false
   | After_rules ->
      local_rules ();
      flag ["ocaml"; "compile"] (S[A"-w"; A"+A-4-40-42-44-48"]);
      if js_of_ocaml_version >= (3, 6) then
        flag ["js_of_ocaml"] & S[A"+js_of_ocaml-compiler/runtime.js"];
      (match Sys.getenv "TERM" with
       | exception Not_found -> ()
       | "" | "dumb" -> ()
       | _ -> flag ["ocaml"; "compile"] (S [A"-color"; A"always"]))
   | _ -> ())
