#! /usr/bin/env ocaml
#use "topfind"
#require "adpkg"
#require "topkg"
#require "unix"

open Adpkg
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

let opams = [
  Pkg.opam_file
    ~lint:false (* until 1.2 support is dropped *)
    ~lint_deps_excluding:(Some [
      "adpkg"; "findlib"; "ocamlfind"; "ounit"; "oUnit";
    ])
    "subsocia-eliom.opam"
]

let build_cmd c os targets =
  let ocamlbuild = Conf.tool "ocamlbuild" os in
  let build_dir = Conf.build_dir c in
  OS.Cmd.run @@
  Cmd.(ocamlbuild
        % "-use-ocamlfind"
        % "-plugin-tag" % "package(eliom.ocamlbuild)"
        % "-build-dir" % build_dir
        % "ocsigen-dev.conf"
        %% of_list targets)

let build = Pkg.build ~cmd:build_cmd ()

let () = Pkg.describe "subsocia-eliom" ~licenses ~opams ~build @@ fun c ->
  Modules.of_file "web/server/subsocia-eliom-server.oclib"
    >>= fun web_server_modules ->
  Modules.of_file "web/server/subsocia-eliom-module.oclib"
    >>= fun web_module_modules ->
  Modules.of_file "web/server/subsocia-eliom-debug-module.oclib"
    >>= fun web_debug_module_modules ->
  Modules.mllib web_server_modules
    ~dst_dir:"web/server/" "web/server/subsocia-eliom-server.mllib"
    >>= fun web_server_mllib ->
  Modules.mllib web_module_modules
    ~dst_dir:"web/server/" "web/server/subsocia-eliom-module.mllib"
    >>= fun web_module_mllib ->
  Modules.mllib web_debug_module_modules
    ~dst_dir:"web/server/" "web/server/subsocia-eliom-debug-module.mllib"
    >>= fun web_debug_module_mllib ->
  let mllibs = [
    web_server_mllib;
    web_module_mllib;
    web_debug_module_mllib;
  ] in
  let doc_modules =
    let (++) = Modules.union in
    web_server_modules ++ web_module_modules ++ web_debug_module_modules in
  Modules.save doc_modules "doc/api.odocl"
               ~filter:Filter.(not (tagged "internal")) >>= fun () ->
  Modules.save doc_modules "doc/dev.odocl" >>= fun () ->
  let misc_fields = [
    Pkg.share ~dst:"static/" "web/client/sociaweb_app.js";
    Pkg.share ~dst:"static/css/" "web/static/css/subsocia.css";
  ] in
  Ok (mllibs @ misc_fields)
