(* Copyright (C) 2016--2023  Petter A. Urkedal <paurkedal@gmail.com>
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

[%%server.start]
open Subsocia_connection

module Sociaweb_app : Eliom_registration.APP

val entity_for_view : operator: Entity.t -> int32 -> Entity.t Lwt.t
val entity_for_edit : operator: Entity.t -> int32 -> Entity.t Lwt.t

val force_dsub : operator: Entity.t -> int32 * int32 -> unit Lwt.t
val relax_dsub : operator: Entity.t -> int32 * int32 -> unit Lwt.t

[%%client.start]

val force_dsub : int32 * int32 -> unit Lwt.t
val relax_dsub : int32 * int32 -> unit Lwt.t
(*
val candidate_dsupers : int32 -> (string * int32) list Lwt.t
*)
