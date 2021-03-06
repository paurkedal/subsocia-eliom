(* Copyright (C) 2015--2020  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Lwt.Infix
open Subsocia_connection
open Subsocia_selector
open Unprime_option

type authenticalia = {
  auth_method : string;
  auth_identity : string;
}

let authentication_hook = ref []
let updating_autoreg_hook = ref []
let oneshot_autoreg_hook = ref []

module Log_auth = struct
  let section = Lwt_log.Section.make "subsocia.auth"
  let debug_f fmt = Lwt_log.debug_f ~section fmt
end

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

let get_authenticalia_opt () =
  match%lwt Pwt_list.search_s (fun p -> p ()) !authentication_hook with
  | Some _ as r -> Lwt.return r
  | None ->
    let ri = Eliom_request_info.get_ri () in
    let frame = Ocsigen_extensions.Ocsigen_request_info.http_frame ri in
    let auth_method =
      match Sociaweb_config.hba_method_header#get with
      | None -> None
      | Some hn -> Option.found (fun () -> Ocsigen_headers.find hn frame)
    in
    let auth_method =
      match auth_method with
      | None -> Sociaweb_config.hba_method#get
      | Some _ as r -> r
    in
    let auth_identity =
      match Sociaweb_config.hba_identity_header#get with
      | None -> None
      | Some hn -> Option.found (fun () -> Ocsigen_headers.find hn frame)
    in
    match auth_method, auth_identity with
    | Some auth_method, Some auth_identity ->
      Lwt.return (Some {auth_method; auth_identity})
    | None, _ | _, None ->
      Lwt.return_none

let get_authenticalia () =
  match%lwt get_authenticalia_opt () with
  | Some r -> Lwt.return r
  | None -> http_error 401 "Not authenticated."

let auth_top =
  let en = Sociaweb_config.auth_top#get in
  match%lwt Entity.select_opt (selector_of_string en) with
  | None -> Lwt.fail (Failure ("Missing configured auth group "^en^"."))
  | Some e -> Lwt.return e

let auth_method_group name =
  let%lwt ag = auth_top in
  Entity.of_unique_name ~super:ag name

let entity_of_authenticalia auth =
  match%lwt auth_method_group auth.auth_method with
  | None -> Lwt.return_none
  | Some amg ->
    let%lwt at_unique_name = Const.at_unique_name in
    let%lwt s = Entity.image1_eq at_unique_name auth.auth_identity amg in
    match Entity.Set.cardinal s with
    | 1 -> Lwt.return (Some (Entity.Set.min_elt_exn s))
    | 0 -> Lwt.return_none
    | _ -> http_error 500 "Duplicate registration."

let set_authenticalia subject auth =
  match%lwt auth_method_group auth.auth_method with
  | None -> http_error 500 "Missing group for authentication method."
  | Some amg ->
    let%lwt at_unique_name = Const.at_unique_name in
    Entity.force_sub subject amg >>= fun () ->
    Entity.set_value at_unique_name auth.auth_identity amg subject

let autoreg_entity_of_authenticalia auth =
  match%lwt Pwt_list.search_s (fun p -> p auth) !updating_autoreg_hook with
  | Some _ as r -> Lwt.return r
  | None ->
    match%lwt entity_of_authenticalia auth with
    | Some _ as r -> Lwt.return r
    | None -> Pwt_list.search_s (fun p -> p auth) !oneshot_autoreg_hook

let get_operator_opt () =
  match%lwt get_authenticalia_opt () with
  | None ->
    Log_auth.debug_f "Not authenticated." >>= fun () ->
    Lwt.return_none
  | Some auth ->
    Log_auth.debug_f "Authenicated %s with %s."
                     auth.auth_identity auth.auth_method >>= fun () ->
    autoreg_entity_of_authenticalia auth

let get_operator () =
  let%lwt auth = get_authenticalia () in
  match%lwt autoreg_entity_of_authenticalia auth with
  | Some e -> Lwt.return e
  | None -> http_error 403 "Not registered."

let authenticate () =
  let%lwt user = get_operator () in
  let%lwt user_id = Entity.soid user in
  let session_id = "user_id=" ^ (Entity.Soid.to_string user_id) in
  let scope = Eliom_common.default_session_scope in
  let%lwt () = Eliom_state.set_persistent_data_session_group ~scope session_id in
  Eliom_state.set_service_session_group ~scope session_id;
  Eliom_state.set_volatile_data_session_group ~scope session_id;
  Lwt.return user
