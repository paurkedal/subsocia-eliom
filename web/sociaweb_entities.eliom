(* Copyright (C) 2014--2023  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

open%shared Eliom_content.Html
open%client Js_of_ocaml
open%shared Lwt.Infix
open%server Lwt.Syntax
open%server Subsocia_common
open%server Subsocia_connection
open%server Subsocia_selector
open%server Unprime

open%server Sociaweb_auth
open%server Sociaweb_request
open%shared Sociaweb_content
open%shared Sociaweb_server
open%shared Sociaweb_services

let collate x y = Confero.collate ~total:true x y

module Int32_hashable = struct
  type t = int32
  let equal = (=)
  let hash = Hashtbl.hash
end
module Int32_event_table = Panograph_event_table.Make (Int32_hashable)

let entity_changed =
  let event_table = Int32_event_table.create 97 in
  on_entity_change begin function
   | `Force_dsub (e1, e2) | `Relax_dsub (e1, e2) | `Change_values (e1, e2) ->
      Lwt.async begin fun () ->
        let+ e1_id = Entity.soid e1 and+ e2_id = Entity.soid e2 in
        Int32_event_table.emit event_table e1_id ();
        Int32_event_table.emit event_table e2_id ()
      end
  end;
  Int32_event_table.event event_table

let suggested_number_of_columns es =
  let n = List.length es in
  if n = 0 then 1 else
  let ws = List.map (fun (name, _) -> String.length name) es in
  let ws = List.sort (fun x y -> compare y x) ws in
  let w = List.nth ws ((n - 1) / 8) in
  min (max 1 (120 / w)) 8

let ordered_entities ~cri es =
  let amend_name e =
    let+ name = Entity.display_name ~langs:cri.cri_langs e in
    (name, e)
  in
  let+ es = Lwt_list.map_s amend_name (Entity.Set.elements es) in
  let es = List.sort (fun (s0, _) (s1, _) -> collate s0 s1) es in
  List.map snd es, suggested_number_of_columns es

(* Directory *)

let shortest s1 s2 = if String.length s1 < String.length s2 then s1 else s2

let pick_shortest = function
 | [] -> assert false
 | [text] -> text
 | text :: texts -> List.fold_left shortest text texts

let render_directory ~cri ~display_name entity =
  let* paths = Entity.paths entity in
  let render_path_li path =
    F.li [
      F.code ~a:[F.a_class ["soc-path"]] [F.txt (string_of_selector path)];
    ]
  in
  let extract_adjacency (neigh, rels) =
    let* sels = Lwt_list.map_p Relation.to_selector rels in
    let* neigh_id = Entity.soid neigh in
    let+ neigh_name = Entity.display_name ~langs:cri.cri_langs neigh in
    let arrow_label = sels |> List.map string_of_selector |> pick_shortest in
    let neigh_link =
      F.a ~service:entities_service [F.txt neigh_name] (Some neigh_id)
    in
    (arrow_label, neigh_link)
  in
  let postprocess_adjacencies rels_by_e =
    rels_by_e
      |> Entity.Map.bindings
      |> Lwt_list.map_p extract_adjacency
      >|= List.sort (fun (a, _) (b, _) -> Confero.collate ~total:true a b)
  in
  let render_arrow arrow_label =
    F.span ~a:[F.a_class ["soc-arrow"]] [
      F.span ~a:[F.a_class ["soc-label"]] [F.txt arrow_label];
    ]
  in
  let render_preimage (arrow_label, source_link) =
    F.div [source_link; render_arrow arrow_label];
  in
  let render_image (arrow_label, target_link) =
    F.div [render_arrow arrow_label; target_link]
  in
  let* unique_preimages =
    Entity.unique_relations_by_preimage entity
      >>= postprocess_adjacencies
      >|= List.map render_preimage
  in
  let+ unique_images =
    Entity.unique_relations_by_image entity
      >>= postprocess_adjacencies
      >|= List.map render_image
  in
  F.div ~a:[F.a_class ["soc-directory"]] [
    F.table ~a:[F.a_class ["soc-arrows"]] [
      F.tr [
        F.td ~a:[F.a_class ["source"]] unique_preimages;
        F.td ~a:[F.a_class ["focus"]] [
          F.p [F.txt display_name];
          F.ul ~a:[F.a_class ["soc-paths"]] (List.map render_path_li paths);
        ];
        F.td ~a:[F.a_class ["target"]] unique_images;
      ]
    ]
  ]

(* Attributions *)

let rec fold_closure_from f dsucc x acc =
  let* xs = dsucc x in
  let+ acc = Entity.Set.fold_s (fold_closure_from f dsucc) xs acc in
  f x acc

let render_attribution ~cri target source =
  let* target_type = Entity.entity_type target in
  let* source_type = Entity.entity_type source in
  let* ats = Entity_type.allowed_attributes source_type target_type in
  let ats = Attribute_type.Set.elements ats in
  let render_tr (Attribute_type.Any at) =
    let* an = Attribute_type.name at in
    let+ value_frag =
      let+ values = Entity.get_values at source target in
      (match Values.elements values with
       | [] ->
          [F.span ~a:[F.a_class ["soc-value"; "none"]] [F.txt "-"]]
       | values ->
          let t = Attribute_type.value_type at in
          let render_value v = F.li [F.txt (Value.typed_to_string t v)] in
          [F.ul ~a:[F.a_class ["soc-values"]] (List.map render_value values)])
    in
    let mult_class =
      (match Attribute_type.value_mult at with
       | May1 -> "may1" | May -> "may"
       | Must1 -> "must1" | Must -> "must")
    in
    F.tr ~a:[F.a_class [mult_class]] [F.td [F.txt an]; F.td value_frag]
  in
  let* attr_trs = Lwt_list.map_s render_tr ats in
  let* source_type_name = Entity_type.name source_type in
  let+ source_type_link = entity_link ~langs:cri.cri_langs source in
  if attr_trs = [] then
    None
  else
    let htr = F.tr [F.th [F.txt source_type_name]; F.th [source_type_link]] in
    Some (htr :: attr_trs)

let render_attributions ~cri ent =
  let attr_aux ub acc =
    render_attribution ~cri ent ub
      >|= function None -> acc | Some trs -> trs :: acc
  in
  let* ubs =
    fold_closure_from
      Entity.Set.add (Entity.preimage1 Relation.True) ent Entity.Set.empty
  in
  let+ attr_trss = Entity.Set.fold_s attr_aux ubs [] in
  let sep_tr = F.tr ~a:[F.a_class ["soc-sep"]] [F.td ~a:[F.a_colspan 2] []] in
  if attr_trss = [] then
    F.p [F.txt "There are now incoming attributions."]
  else
    F.table ~a:[F.a_class ["soc-attributions"]]
      (List.flatten (Prime_list.interfix [sep_tr] attr_trss))

(* Inclusions *)

let neighbour_link ~cri ent =
  entity_link ~langs:cri.cri_langs ent >|= fun x -> [x]

let render_dsuper_ro ~cri focus =
  let* dsupers = Entity.dsuper focus in
  let* dsupers, m = ordered_entities ~cri dsupers in
  let+ dsuper_frags = Lwt_list.map_s (neighbour_link ~cri) dsupers in
  multicol ~m ~cls:["soc-dsuper1"] dsuper_frags

let neighbour_link_rw ~cri ~focus dsuper =
  let* can_edit = Entity.can_edit_entity cri.cri_operator dsuper in
  let* link = entity_link ~langs:cri.cri_langs dsuper in
  if can_edit then
    let* focus_id = Entity.soid focus in
    let+ dsuper_id = Entity.soid dsuper in
    let remove_button =
      let handler =
        [%client fun _ ->
          Lwt.async (fun () -> relax_dsub (~%focus_id, ~%dsuper_id))]
      in
      F.button ~a:[F.a_button_type `Button; F.a_onclick handler] [F.txt "-"]
    in
    F.td [remove_button; link]
  else
    let remove_button =
      F.button
        ~a:[F.a_button_type `Button; F.a_disabled ();
            F.a_style "visibility: hidden"] []
    in
    Lwt.return (F.td [remove_button; link])

let candidate_dsupers ~cri focus =
  let operator = cri.cri_operator in
  let extract entity =
    let* name = Entity.display_name ~langs:cri.cri_langs entity in
    let+ id = Entity.soid entity in
    (name, id)
  in
  let compare (name1, id1) (name2, id2) =
    let c = Confero.collate ~total:true name1 name2 in
    if c <> 0 then c else Int32.compare id1 id2
  in
  Entity.candidate_dsupers focus
    >>= Entity.Set.filter_s (Entity.can_edit_entity operator)
    >|= Entity.Set.elements
    >>= Lwt_list.map_p extract
    >|= List.sort compare

let render_dsuper_add ~cri focus =
  let* focus_id = Entity.soid focus in
  let+ csupers = candidate_dsupers ~cri focus in
  let render_option (name, id) =
    F.option ~a:[F.a_value (Int32.to_string id)] (F.txt name)
  in
  let select =
    D.select (
      F.option ~a:[F.a_value "0"] (F.txt "") ::
      List.map render_option csupers
    )
  in
  let handler =
    [%client
      let select_dom = To_dom.of_select ~%select in
      fun _ ->
        let csuper_id = Int32.of_string (Js.to_string select_dom##.value) in
        select_dom##.value := Js.string "0";
        if csuper_id <> 0l then
          Lwt.async (fun () -> force_dsub (~%focus_id, csuper_id))
    ]
  in
  let button =
    F.button ~a:[F.a_button_type `Button; F.a_onclick handler] [F.txt "+"]
  in
  [button; select]

let render_dsuper_rw ~cri focus =
  let* dsupers = Entity.dsuper focus in
  let* dsupers, m = ordered_entities ~cri dsupers in
  let* dsuper_add = render_dsuper_add ~cri focus in
  let+ dsuper_frags =
    Lwt_list.rev_map_p (neighbour_link_rw ~cri ~focus) dsupers
    >|= List.rev % List.cons (F.td dsuper_add)
  in
  multicol_tds ~m ~cls:["soc-dsuper1"] dsuper_frags

let render_dsuper ~cri ~enable_edit focus =
  if enable_edit then
    render_dsuper_rw ~cri focus
  else
    render_dsuper_ro ~cri focus

let render_inclusions ~cri ?(enable_edit = true) ent =
  let* dsub, m = Entity.dsub ent >>= ordered_entities ~cri in
  let* dsub_frags = Lwt_list.map_s (neighbour_link ~cri) dsub in
  let* name = Entity.display_name ~langs:cri.cri_langs ent in
  let+ dsuper_frag = render_dsuper ~cri ~enable_edit ent in
  F.div ~a:[F.a_class ["soc-entity-browser"]] [
    dsuper_frag;
    F.div ~a:[F.a_class ["soc-box"; "focus"; "top"]] [F.txt name];
    F.div ~a:[F.a_class ["soc-box"; "focus"; "middle"; "content"]]
          [];
    F.div ~a:[F.a_class ["soc-box"; "focus"; "bottom"; "content"]]
          [multicol ~m ~cls:["soc-dsub1"] dsub_frags];
  ]

(* Handlers *)

let default_entity_sel =
  selector_of_string Sociaweb_config.(global.default_entity)

let entity_handler entity_id_opt () =
  let* cri = authenticate_cri () in
  let* entity_id =
    (match entity_id_opt with
     | Some id -> Lwt.return id
     | None ->
        let* entity_id =
          (Entity.select_opt default_entity_sel >>= function
           | None -> Lwt.return 1l
           | Some entity -> Entity.soid entity)
        in
        http_redirect ~service:entities_service (Some entity_id))
  in
  let* e = entity_for_view ~operator:cri.cri_operator entity_id in
  let* enable_edit =
    (* TODO: Infer from permissions or explicit request. *)
    (match Sociaweb_config.(global.member_types) with
     | [] -> Lwt.return_true
     | ets ->
        Entity.entity_type e
          >>= Entity_type.name
          >|= (fun et -> List.mem et ets))
  in
  let* display_name = Entity.display_name ~langs:cri.cri_langs e in
  let* directory_div = render_directory ~cri ~display_name e in
  let* inclusions_div = render_inclusions ~enable_edit ~cri e in
  let* attributions_div = render_attributions ~cri e in
  let entity_changed_c = Eliom_react.Down.of_react (entity_changed entity_id) in
  ignore_cv [%client
    Lwt_react.E.keep @@ React.E.trace
      (fun _ ->
        Eliom_client.exit_to ~service:Eliom_service.reload_action () ())
      ~%entity_changed_c
  ];
  let do_search = [%client function
   | None -> Lwt_result.return ()
   | Some (_, entity_id) ->
      Eliom_client.change_page ~service:entities_service (Some entity_id) ()
        >>= fun () ->
      Lwt_result.return ()
  ] in
  let+ search_inp, _search_handle = entity_completion_input do_search in
  let search_div = F.div ~a:[F.a_class ["soc-search"]] [
    F.label [F.txt "Search"]; F.br ();
    search_inp;
  ] in
  Eliom_tools.D.html
    ~title:"Entity Browser"
    ~css:[["css"; "subsocia.css"]; ["css"; "panograph.css"]]
    (D.body [
      search_div;

      F.h1 [F.txt display_name];

      F.h2 [F.txt "Directory"];
      directory_div;

      F.h2 [F.txt "Inclusion Diagram"];
      inclusions_div;

      F.h2 [F.txt "Incoming Attributes"];
      attributions_div;
    ])

let entities_self_handler () () =
  let* operator = authenticate () in
  let+ operator_id = Entity.soid operator in
  Eliom_registration.Redirection
    (Eliom_service.preapply ~service:entities_service (Some operator_id))

let () =
  let open Eliom_registration in
  Sociaweb_app.register ~service:entities_service entity_handler;
  Redirection.register ~service:entities_self_service entities_self_handler
