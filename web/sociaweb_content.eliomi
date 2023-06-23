(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Web helpers. *)

[%%shared.start]
open Eliom_content

[%%server.start]
open Iso639
open Subsocia_connection

val ignore_cv : unit Eliom_client_value.t -> unit

val entity_link :
  langs: Lang.t list -> Entity.t ->
  [> [> Html_types.txt] Html_types.a] Html.elt Lwt.t

[%%shared.start]

val multicol_tds :
  ?m: int -> ?cls: string list ->
  [< Html_types.tr_content_fun > `Td] Html.elt list ->
  [> Html_types.table | Html_types.div] Html.elt

val multicol :
  ?m: int -> ?cls: string list ->
  [< Html_types.td_content] Html.elt list list ->
  [> Html_types.table | Html_types.div] Html.elt

[%%server.start]

val entity_completion_input :
  ?entity_type: Entity_type.t ->
  ?super: Entity.t ->
  ((string * int32) option -> unit Panui_result.t Lwt.t) Eliom_client_value.t ->
  ([> Html_types.span] Html.elt *
   (string * int32) option Panui_complete.handle Eliom_client_value.t) Lwt.t

[%%client.start]

val entity_completion_input :
  ?entity_type_id: int32 ->
  ?super_id: int32 ->
  ((string * int32) option -> unit Panui_result.t Lwt.t) ->
  [> Html_types.span] Html.elt * (string * int32) option Panui_complete.handle
