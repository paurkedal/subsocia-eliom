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

open Lwt.Infix
open Lwt.Syntax
open Sociaweb_logging
open Sociaweb_request
open Subsocia_connection

module Sociaweb_app = Eliom_registration.App
  (struct
    let application_name = "sociaweb_app"
    let global_data_path = None
  end)

let entity_for_view ~operator entity_id =
  let* entity = Entity.of_soid entity_id in
  Entity.can_view_entity operator entity >>= function
   | true -> Lwt.return entity
   | false -> http_error 403 "Not authorized for this entity."

let entity_for_edit ~operator entity_id =
  let* entity = Entity.of_soid entity_id in
  Entity.can_edit_entity operator entity >>= function
   | true -> Lwt.return entity
   | false -> http_error 403 "Not authorized for editing this entity."

let force_dsub ~operator (lb_id, ub_id) =
  let* ub = entity_for_edit ~operator ub_id in
  let* lb = entity_for_view ~operator lb_id in
  let* ub_name = Entity.display_name ub in
  let* lb_name = Entity.display_name lb in
  let* editor_id = Entity.soid operator in
  let* editor_name = Entity.display_name operator in
  Change_log.info (fun f ->
    f "#%ld %S forces #%ld %S ⊆ #%ld %S"
      editor_id editor_name lb_id lb_name ub_id ub_name) >>= fun () ->
  Entity.force_dsub lb ub

let relax_dsub ~operator (lb_id, ub_id) =
  let* ub = entity_for_edit ~operator ub_id in
  let* lb = entity_for_view ~operator lb_id in
  let* ub_name = Entity.display_name ub in
  let* lb_name = Entity.display_name lb in
  let* editor_id = Entity.soid operator in
  let* editor_name = Entity.display_name operator in
  Change_log.info (fun f ->
    f "#%ld %S relaxes #%ld %S ⊆ #%ld %S"
      editor_id editor_name lb_id lb_name ub_id ub_name) >>= fun () ->
  Entity.relax_dsub lb ub

let%client force_dsub = ~%(auth_sf [%json: int32 * int32] force_dsub)
let%client relax_dsub = ~%(auth_sf [%json: int32 * int32] relax_dsub)
