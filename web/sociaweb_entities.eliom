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

[%%shared
  open Lwt.Infix

  open Sociaweb_content
  open Sociaweb_server
  open Sociaweb_services
]
[%%server
  open Eliom_content.Html
  open Lwt.Syntax
  open Subsocia_common
  open Subsocia_connection
  open Subsocia_selector

  open Sociaweb_auth
  open Sociaweb_request
]

[%%server

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

  let neighbour_link ~cri ent =
    entity_link ~langs:cri.cri_langs ent >|= fun x -> [x]

  let neighbour_with_edit ~cri focus csuper =
    let* can_edit = Entity.can_edit_entity cri.cri_operator csuper in
    let* link = entity_link ~langs:cri.cri_langs csuper in
    if not can_edit then
      let button =
        F.button
          ~a:[F.a_button_type `Button; F.a_disabled ();
              F.a_style "visibility: hidden"] []
      in
      Lwt.return (F.td [button; link])
    else
      let* focus_id = Entity.soid focus in
      let* dsuper_id = Entity.soid csuper in
      let+ is_dsuper = Entity.is_dsub focus csuper in
      let label, handler, a =
        if is_dsuper then
          let remove = [%client fun _ ->
            Lwt.async (fun () -> relax_dsub (~%focus_id, ~%dsuper_id))] in
          ("-", remove, None)
        else
          let add = [%client fun _ ->
            Lwt.async (fun () -> force_dsub (~%focus_id, ~%dsuper_id))] in
          ("+", add, Some [F.a_class ["candidate"]])
      in
      let button =
        F.button
          ~a:[F.a_button_type `Button; F.a_onclick handler]
          [F.txt label]
      in
      F.td ?a [button; link]

  let rec fold_closure_from f dsucc x acc =
    let* xs = dsucc x in
    let+ acc = Entity.Set.fold_s (fold_closure_from f dsucc) xs acc in
    f x acc

  let upwards_closure e =
    fold_closure_from
      Entity.Set.add (Entity.preimage1 Relation.True) e Entity.Set.empty

  let render_attribution ~cri lb ub =
    let* lbt = Entity.entity_type lb in
    let* ubt = Entity.entity_type ub in
    let* ats = Entity_type.allowed_attributes ubt lbt in
    let ats = Attribute_type.Set.elements ats in
    let render_tr (Attribute_type.Any at) =
      let* an = Attribute_type.name at in
      let mu = Attribute_type.value_mult at in
      let* value_frag =
        let t1 = Attribute_type.value_type at in
        Entity.get_values at ub lb >|= fun vs ->
        (match List.map (Value.typed_to_string t1) (Values.elements vs) with
         | [] -> [F.span ~a:[F.a_class ["none"]] [F.txt "-"]]
         | strs -> [F.txt (String.concat ", " strs)])
      in
      Lwt.return @@ F.tr [
        F.td [F.txt an; F.sup [F.txt (Multiplicity.to_string mu)];
              F.txt ":"];
        F.td value_frag;
      ]
    in
    let* attr_trs = Lwt_list.map_s render_tr ats in
    let+ ub_link = entity_link ~langs:cri.cri_langs ub in
    if attr_trs = [] then
      None
    else
      Some (F.tr [F.td []; F.th [ub_link]] :: attr_trs)

  let render_dsuper ~cri ~enable_edit focus =
    let is_relevant csuper =
      let* is_dsuper = Entity.is_dsub focus csuper in
      if is_dsuper then Lwt.return_true else
      Entity.can_edit_entity cri.cri_operator csuper
    in
    if enable_edit then
      let* csupers = Entity.candidate_dsupers ~include_current:true focus in
      let* csupers = Entity.Set.filter_s is_relevant csupers in
      let* csupers, m = ordered_entities ~cri csupers in
      let+ csuper_frags =
        Lwt_list.map_s (neighbour_with_edit ~cri focus) csupers in
      let csuper_block = multicol_tds ~m ~cls:["soc-dsuper1"] csuper_frags in
      F.table ~a:[F.a_class ["soc-layout"]]
        [F.tr [F.th [F.txt "Membership Management"]];
         F.tr [F.td [csuper_block]]]
    else
      let* dsupers = Entity.dsuper focus in
      let* dsupers, m = ordered_entities ~cri dsupers in
      let+ dsuper_frags = Lwt_list.map_s (neighbour_link ~cri) dsupers in
      let dsuper_block = multicol ~m ~cls:["soc-dsuper1"] dsuper_frags in
      F.table ~a:[F.a_class ["soc-layout"]]
        [F.tr [F.th [F.txt "Member of"]];
         F.tr [F.td [dsuper_block]]]

  let render_browser ~cri ?(enable_edit = true) ent =
    let* dsub, m = Entity.dsub ent >>= ordered_entities ~cri in
    let* dsub_frags = Lwt_list.map_s (neighbour_link ~cri) dsub in
    let* name = Entity.display_name ~langs:cri.cri_langs ent in
    let* ubs = upwards_closure ent in
    let attr_aux ub acc =
      render_attribution ~cri ent ub
        >|= function None -> acc | Some trs -> trs :: acc
    in
    let* attr_trss = Entity.Set.fold_s attr_aux ubs [] in
    let attr_table =
      F.table ~a:[F.a_class ["soc-assoc"]] (List.flatten attr_trss)
    in
    let+ dsuper_frag = render_dsuper ~cri ~enable_edit ent in
    F.div ~a:[F.a_class ["soc-entity-browser"]] [
      dsuper_frag;
      F.div ~a:[F.a_class ["soc-box"; "focus"; "top"]] [F.txt name];
      F.div ~a:[F.a_class ["soc-box"; "focus"; "middle"; "content"]]
            [attr_table];
      F.div ~a:[F.a_class ["soc-box"; "focus"; "bottom"; "content"]]
            [multicol ~m ~cls:["soc-dsub1"] dsub_frags];
    ]
]

(* TODO: Set enable_edit from permissions or explicit request. *)

let default_entity_sel =
  selector_of_string Sociaweb_config.default_entity#get

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
    (match Sociaweb_config.member_types#get with
     | [] -> Lwt.return_true
     | ets ->
        Entity.entity_type e
          >>= Entity_type.name
          >|= (fun et -> List.mem et ets))
  in
  let* browser_div = render_browser ~enable_edit ~cri e in
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
    (D.body [search_div; browser_div])

let entities_self_handler () () =
  let* operator = authenticate () in
  let+ operator_id = Entity.soid operator in
  Eliom_registration.Redirection
    (Eliom_service.preapply ~service:entities_service (Some operator_id))

let () =
  let open Eliom_registration in
  Sociaweb_app.register ~service:entities_service entity_handler;
  Redirection.register ~service:entities_self_service entities_self_handler
