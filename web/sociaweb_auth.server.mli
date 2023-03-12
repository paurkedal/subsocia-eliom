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

open Subsocia_connection

type request_info = Ocsigen_extensions.Ocsigen_request_info.request_info

type identity_material = {
  prefix: Entity.t;
  attribute: string Attribute_type.t;
  value: string;
}

type authenticator_result =
  | Authenticated of Entity.t
  | Unregistered of identity_material
  | Unauthenticated
  | Terminate of string

val authentication_hook :
  (request_info -> authenticator_result Lwt.t) list ref
val post_authentication_hook :
  (request_info -> authenticator_result -> authenticator_result Lwt.t) list ref
val registration_hook :
  (request_info -> identity_material -> authenticator_result Lwt.t) list ref

val get_authenticator_result : unit -> authenticator_result Lwt.t

val get_operator_opt : unit -> Entity.t option Lwt.t

val get_operator : unit -> Entity.t Lwt.t

val authenticate : unit -> Entity.t Lwt.t
