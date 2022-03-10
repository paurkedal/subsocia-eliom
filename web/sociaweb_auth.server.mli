(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open Subsocia_connection

type authenticalia = {
  auth_method : string;
  auth_identity : string;
}

val authentication_hook : (unit -> authenticalia option Lwt.t) list ref
val updating_autoreg_hook : (authenticalia -> Entity.t option Lwt.t) list ref
val oneshot_autoreg_hook : (authenticalia -> Entity.t option Lwt.t) list ref

val get_authenticalia_opt : unit -> authenticalia option Lwt.t

val get_authenticalia : unit -> authenticalia Lwt.t

val entity_of_authenticalia : authenticalia -> Entity.t option Lwt.t

val set_authenticalia : Entity.t -> authenticalia -> unit Lwt.t

val autoreg_entity_of_authenticalia : authenticalia -> Entity.t option Lwt.t

val get_operator_opt : unit -> Entity.t option Lwt.t

val get_operator : unit -> Entity.t Lwt.t

val authenticate : unit -> Entity.t Lwt.t
