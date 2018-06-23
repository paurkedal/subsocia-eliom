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

let () = Ocamlbuild_plugin.dispatch @@ fun hook ->
  M.dispatcher ~oasis_executables hook;
  (match hook with
   | Before_options -> Options.make_links := false
   | After_rules ->
      local_rules ();
      flag ["ocaml"; "compile"] (S[A"-w"; A"+A-4-42-44-48"]);
      (match Sys.getenv "TERM" with
       | exception Not_found -> ()
       | "" | "dumb" -> ()
       | _ -> flag ["ocaml"; "compile"] (S [A"-color"; A"always"]))
   | _ -> ())
