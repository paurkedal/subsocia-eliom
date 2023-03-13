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
open Subsocia_common
open Subsocia_connection

module Log = (val Logs_lwt.src_log (Logs.Src.create "sociaweb.auth"))

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

let rec chain_authenticators f = function
 | [] -> Lwt.return Unauthenticated
 | arg :: args ->
    (f arg >>= function
     | Authenticated _ | Unregistered _ | Terminate _ as r -> Lwt.return r
     | Unauthenticated -> chain_authenticators f args)

let authentication_hook = ref []
let post_authentication_hook = ref []
let registration_hook = ref []

let http_error code msg =
  Lwt.fail (Ocsigen_http_frame.Http_error.Http_exception (code, Some msg, None))

let authentication_rules =
  let open Sociaweb_config in
  let resolve_path_templ {prefix; attribute; value_pattern; value_template} =
    let* prefix = Entity.select_one prefix in
    let+ attribute = Attribute_type.of_name_exn Type.String attribute in
    {prefix; attribute; value_pattern; value_template}
  in
  let resolve_meth = function
   | Fixed {identity} ->
      let+ identity = Entity.select_one identity in
      Fixed {identity}
   | Trusted_header {header; identity} ->
      let+ identity = resolve_path_templ identity in
      Trusted_header {header; identity}
   | Trusted_environment {variable; identity} ->
      let+ identity = resolve_path_templ identity in
      Trusted_environment {variable; identity}
   | Bearer_jwt {jwk; identity} ->
      let+ identity = resolve_path_templ identity in
      Bearer_jwt {jwk; identity}
  in
  let resolve_rule (cond, meth) =
    let+ meth = resolve_meth meth in
    (cond, meth)
  in
  Lwt_main.run (Lwt_list.map_p resolve_rule global.authentication_rules)
[@@warning "-45"]

let denote_path_template template input_value =
  let open Sociaweb_config in
  (match Re.exec_opt template.value_pattern input_value with
   | None -> Lwt.return Unauthenticated
   | Some g ->
      let buf = Buffer.create 64 in
      let lookup var = Re.Group.get g (int_of_string var) in
      let output_value =
        (try
          Buffer.add_substitute buf lookup template.value_template;
          Buffer.contents buf
         with
         | Failure _ ->
            failwith "In auth config, RE group reference must be integer."
         | Not_found ->
            failwith "In auth config, RE group reference out of range.")
      in
      let extract_identity = function
       | [] ->
          let identity_material = {
            prefix = template.prefix;
            attribute = template.attribute;
            value = output_value;
          } in
          Lwt.return (Unregistered identity_material)
       | [e] ->
          Lwt.return (Authenticated e)
       | _ ->
          Log.err (fun f ->
            f "Ambiguous identity for %s ~> %s." input_value output_value)
            >>= fun () ->
          Lwt.return (Terminate "Multiple registrations.")
      in
      Entity.image1_eq template.attribute output_value template.prefix
        >|= Entity.Set.elements
        >>= extract_identity)
[@@warning "-45"]

let rec denote_condition ~request_info =
  let open Sociaweb_config in
  function
   | Not c -> not (denote_condition ~request_info c)
   | And cs -> List.for_all (denote_condition ~request_info) cs
   | Or cs -> List.exists (denote_condition ~request_info) cs
   | Has_header (header, pattern) ->
      let frame = Ocsigen_request_info.http_frame request_info in
      (match Ocsigen_headers.find header frame with
       | value -> Re.execp pattern value
       | exception Not_found -> false)
   | Has_remote_ip prefix ->
      let address =
        Ipaddr.of_string_exn (Ocsigen_request_info.remote_ip request_info)
      in
      Ipaddr.Prefix.mem address prefix

let auth_jwt jwk identity data =
  (match String.split_on_char ' ' data |> List.filter ((<>) "") with
   | ["Bearer"; token] ->
      let now = Ptime_clock.now () in
      (match Jose.Jwt.of_string ~jwk ~now token with
       | Ok jwt ->
          Log.debug (fun f -> f "Authenticated by JWT.") >>= fun () ->
          (match Jose.Jwt.get_string_claim jwt "sub" with
           | Some sub -> denote_path_template identity sub
           | None ->
              Log.info (fun f -> f "Missing sub claim in JWT.")
              >|= fun () -> Unauthenticated)
       | Error `Expired ->
          Log.info (fun f -> f "The bearer token has expired.")
          >|= fun () -> Unauthenticated
       | Error `Invalid_signature ->
          Log.info (fun f ->
            f "The signature of the bearer token is invalid.")
          >|= fun () -> Unauthenticated
       | Error (`Msg msg) ->
          Log.info (fun f -> f "Bad bearer token: %s" msg)
          >|= fun () -> Unauthenticated
       | Error `Not_json ->
          Log.info (fun f -> f "Bearer token data is not JSON.")
          >|= fun () -> Unauthenticated
       | Error `Not_supported ->
          Log.info (fun f -> f "Bearer token format unsupported.")
          >|= fun () -> Unauthenticated)
   | _ ->
      Log.info (fun f -> f "Authorization header is no a bearer token.")
      >|= fun () -> Unauthenticated)

let denote_authentication_method ~request_info =
  let open Sociaweb_config in
  function
   | Trusted_header {header; identity} ->
      let frame = Ocsigen_request_info.http_frame request_info in
      (match Ocsigen_headers.find header frame with
       | value ->
          Log.debug (fun f -> f "Authenticating %s from trusted header." value)
            >>= fun () ->
          denote_path_template identity value
       | exception Not_found -> Lwt.return Unauthenticated)
   | Trusted_environment {variable; identity} ->
      (match Unix.getenv variable with
       | value ->
          Log.debug (fun f -> f "Authenticated %s from $%s." value variable)
            >>= fun () ->
          denote_path_template identity value
       | exception Not_found -> Lwt.return Unauthenticated)
   | Fixed {identity} ->
      Log.debug (fun f -> f "Authenticating fixed user.") >>= fun () ->
      Lwt.return (Authenticated identity)
   | Bearer_jwt {jwk; identity} ->
      let frame = Ocsigen_request_info.http_frame request_info in
      (match Ocsigen_headers.find "Authorization" frame with
       | exception Not_found -> Lwt.return Unauthenticated
       | data -> auth_jwt jwk identity data)

let denote_authentication_rule ~request_info (cond, meth) =
  if denote_condition ~request_info cond then
    denote_authentication_method ~request_info meth
  else
    Lwt.return Unauthenticated

let get_authenticator_result () =
  let request_info = Eliom_request_info.get_ri () in
  chain_authenticators (fun p -> p request_info) !authentication_hook >>=
  (function
   | Authenticated _ | Unregistered _ | Terminate _ as r ->
      Lwt.return r
   | Unauthenticated ->
      chain_authenticators
        (denote_authentication_rule ~request_info)
        authentication_rules)
  >>= Pwt_list.fold_s (fun p -> p request_info) !post_authentication_hook

let get_operator_opt () =
  (get_authenticator_result () >>= function
   | Authenticated operator -> Lwt.return_some operator
   | Unregistered _ | Unauthenticated | Terminate _ -> Lwt.return_none)

let get_operator () =
  (get_authenticator_result () >>= function
   | Authenticated operator ->
      Lwt.return operator
   | Unregistered identity_material ->
      let request_info = Eliom_request_info.get_ri () in
      let* operator =
        chain_authenticators
          (fun p -> p request_info identity_material)
          !registration_hook
      in
      (match operator with
       | Authenticated operator ->
          Lwt.return operator
       | Unregistered _ ->
          http_error 401 "Not registered."
       | Unauthenticated ->
          http_error 401 "Not authenticated."
       | Terminate msg ->
          http_error 500 msg)
   | Unauthenticated ->
      http_error 401 "Not authenticated."
   | Terminate msg ->
      http_error 500 msg)

let authenticate () =
  let* user = get_operator () in
  let* user_id = Entity.soid user in
  let session_id = "user_id=" ^ (Entity.Soid.to_string user_id) in
  let scope = Eliom_common.default_session_scope in
  let+ () = Eliom_state.set_persistent_data_session_group ~scope session_id in
  Eliom_state.set_service_session_group ~scope session_id;
  Eliom_state.set_volatile_data_session_group ~scope session_id;
  user
