(* Copyright (C) 2015--2023  Petter A. Urkedal <paurkedal@gmail.com>
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
  open Eliom_content.Html
  open Unprime_list
]
[%%server
  open Lwt.Syntax
  open Eliom_client
  open Sociaweb_auth
  open Sociaweb_services
  open Subsocia_connection
]
[%%client
  (* Dummy implementation for the client needed for server-client type checking,
   * since server-only parts are also incleded in .inferred.mli. *)
  module Subsocia_connection = struct
    module Entity = struct
      type soid = int32
    end
    module Entity_type = struct
      type soid = int32
    end
  end
]

[%%server

  let ignore_cv (x : unit Eliom_client_value.t) = ignore x

(*
  let client_node_lwt (m : [`Div] Html.elt Lwt.t client_value) =
    let ph_el = Html.D.div [] in
    ignore {unit{
      let ph_node = Html.To_dom.of_div ~%ph_el in
      Lwt.async @@ fun () ->
      %m >|= fun el ->
      let n = Html.To_dom.of_div el in
      Js.Opt.iter (ph_node##parentNode) (fun p -> Dom.replaceChild p n ph_node)
    }};
    ph_el
*)

  let entity_link ~langs ent =
    let* id = Entity.soid ent in
    let+ name = Entity.display_name ~langs ent in
    F.a ~service:entities_service [F.txt name] (Some id)
]

[%%shared

  let multicol_tds ?(m = 4) ?(cls = []) items =
    let items = Array.of_list items in
    let l = Array.length items in
    let n = (l + m - 1) / m in
    let mktr i =
      let mktd j = if i + j * n < l then items.(i + j * n) else F.td [] in
      F.tr (List.sample mktd m)
    in
    match List.sample mktr n with
    | [] -> F.div ~a:[F.a_class ("multicol" :: "empty" :: cls)] []
    | trs -> F.table ~a:[F.a_class ("multicol" :: cls)] trs

  let multicol ?m ?cls items =
    multicol_tds ?m ?cls (List.map (fun els -> F.td els) items)
]

let complete_helper entity_type_id super_id words_str =
  let* operator = authenticate () in
  let* super = Pwt_option.map_s Entity.of_soid super_id in
  let* can_search =
    (match super with
     | None ->
        let* root = Entity.get_root () in
        Entity.can_search_below operator root
     | Some super ->
        Entity.can_search_below operator super)
  in
  if not can_search then
    Lwt.return (Panui_result.error "Search not permitted.") else
  let* entity_type = Pwt_option.map_s Entity_type.of_soid entity_type_id in
  (match Subsocia_fts.of_completion_string words_str with
   | None -> Lwt_result.return []
   | Some fts ->
      let cutoff = Sociaweb_config.(global.completion_cutoff) in
      let limit = Sociaweb_config.(global.completion_limit) in
      let* root = Entity.get_root () in
      Lwt_result.ok
        @@ Entity.image1_fts ?entity_type ?super ~cutoff ~limit fts root)

let complete (entity_type_id, super_id, words_str) =
  Lwt_result.bind_lwt
    (complete_helper entity_type_id super_id words_str)
    (Lwt_list.map_s
      (fun (e, _) ->
        let* name = Entity.display_name e in
        let+ id = Entity.soid e in
        (name, id)))

let%client complete
  : int32 option * int32 option * string ->
    (string * int32) list Panui_result.t Lwt.t =
  ~%(server_function [%json: int32 option * int32 option * string] complete)

(*
let completed (entity_type_id, super_id, str) =
  let* res =
    Lwt_result.bind_lwt
      (complete_helper entity_type_id super_id str)
      (Lwt_list.filter_s (fun (e, _) -> Entity.display_name e >|= (=) str))
  in
  (match res with
   | Ok [(entity, _)] -> Entity.soid entity >|= Option.some
   | _ -> Lwt.return None)

let%client completed
    : int32 option * int32 option * string -> int32 option Lwt.t =
  ~%(server_function [%json: int32 option * int32 option * string] completed)
*)

let entity_completion_input ?entity_type ?super emit =
  let* entity_type_id = Pwt_option.map_s Entity_type.soid entity_type in
  let+ super_id = Pwt_option.map_s Entity.soid super in
  let complete =
    [%client fun s -> complete (~%entity_type_id, ~%super_id, s)]
  in
  Panui_complete.labelled_int32_option ~complete ~emit None

let%client entity_completion_input ?(entity_type_id : int32 option)
                                   ?(super_id : int32 option) emit =
  let complete s = complete (entity_type_id, super_id, s) in
  Panui_complete.labelled_int32_option ~complete ~emit None
