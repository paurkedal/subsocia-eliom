(* This tool scans the web directory for client code and generates
 * correspending rules in web/dune.client. *)
open Printf

let dirs = [
  "../../../web";
]

let if_chop_suffix nm suffix f =
  if Filename.check_suffix nm suffix then f (Filename.chop_suffix nm suffix)

let gen_eliomi_rule nm =
  printf
    "(rule \
    \ (target %s.mli) \
    \ (deps (:ppx %%{exe:../tools/eliom_ppx_client.exe}) ../%s.eliomi)\n\
    \ (action (with-stdout-to %%{target} (chdir ../..\n\
    \  (run %%{ppx} --as-pp --intf web/%s.eliomi)))))\n"
    nm nm nm

let gen_eliom_rule nm =
  printf
    "(rule \
    \ (target %s.ml) \
    \ (deps (:ppx %%{exe:../tools/eliom_ppx_client.exe}) ../%s.eliom %%{cmo:../%s})\n\
    \ (action (with-stdout-to %%{target} (chdir ../..\n\
    \  (run %%{ppx} --as-pp -server-cmo %%{cmo:../%s} --impl web/%s.eliom)))))\n"
    nm nm nm nm nm
  (* It is important that this rule changes to the toplevel directory when
   * invoking the PPX, since this is the refenece point for file locations used
   * to identify fragments. *)

let gen_copy_rule source_suffix target_suffix nm =
  printf "(rule (copy# ../%s%s %s%s))\n" nm source_suffix nm target_suffix

let gen_rule nm =
  if nm.[0] <> '.'
      && not (Filename.check_suffix nm ".pp.eliom")
      && not (Filename.check_suffix nm ".pp.eliomi")
  then begin
    if_chop_suffix nm ".eliomi" gen_eliomi_rule;
    if_chop_suffix nm ".eliom" gen_eliom_rule;
    if_chop_suffix nm ".client.mli" (gen_copy_rule ".client.mli" ".mli");
    if_chop_suffix nm ".client.ml" (gen_copy_rule ".client.ml" ".ml");
    if_chop_suffix nm ".shared.mli" (gen_copy_rule ".shared.mli" ".mli");
    if_chop_suffix nm ".shared.ml" (gen_copy_rule ".shared.ml" ".ml")
  end

let () =
  Array.concat (List.map Sys.readdir dirs)
    |> Array.to_list
    |> List.sort compare
    |> List.iter gen_rule
