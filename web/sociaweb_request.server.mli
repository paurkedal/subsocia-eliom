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

open Eliom_client
open Iso639
open Subsocia_connection

val http_error : int -> string -> 'a Lwt.t
val http_redirect :
      service: ('get, unit, Eliom_service.get, _, _, _, _,
                _, _, unit, _) Eliom_service.t ->
      'get -> 'noreturn Lwt.t

type custom_request_info = {
  cri_operator : Entity.t;
  cri_langs : Lang.t list;
}

val authenticate_cri : unit -> custom_request_info Lwt.t

val auth_sf : 'a Deriving_Json.t -> (operator: Entity.t -> 'a -> 'b Lwt.t) ->
              ('a, 'b) server_function
