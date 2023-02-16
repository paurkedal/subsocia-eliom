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

open Iso639
open Lwt.Syntax
open Eliom_client
open Sociaweb_auth
open Subsocia_connection
open Unprime_list
open Unprime_option

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

let http_redirect ~service get =
  let uri = Eliom_uri.make_string_uri ~absolute:true ~service get in
  let hdrs = Http_headers.empty |> Http_headers.(add location) uri in
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (302, None, Some hdrs))

(* Request Info *)

let request_info_langs () =
  let compare_al (_, qA) (_, qB) =
    compare (Option.get_or 1.0 qB) (Option.get_or 1.0 qA)
  in
  let decode_al (s, _) = Lang.of_iso639p1 s in
  let als = List.sort compare_al (Eliom_request_info.get_accept_language ()) in
  List.filter_map decode_al als

type custom_request_info = {
  cri_operator : Entity.t;
  cri_langs : Lang.t list;
}

let authenticate_cri () =
  let+ cri_operator = authenticate () in
  let cri_langs =
    (match request_info_langs () with
     | [] -> [Lang.of_string_exn "eng"]
     | langs -> langs)
  in
  {cri_operator; cri_langs}

(* Utility Functions *)

let auth_sf json f =
  let f' tup =
    let* operator = authenticate () in
    f ~operator tup
  in
  server_function json f'
