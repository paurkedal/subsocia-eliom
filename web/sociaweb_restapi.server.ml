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
open Sociaweb_request
open Subsocia_common
open Subsocia_connection
open Subsocia_selector
open Printf
open Unprime_list

let allowed_attributes = lazy
  (List.fold String_set.add Sociaweb_config.(global.restapi_allowed_attributes)
             String_set.empty)

let bprint_json_value buf = function
  | `Bool b -> Buffer.add_string buf (if b then "true" else "false")
  | `Int i -> Buffer.add_string buf (string_of_int i)
  | `String s ->
    Buffer.add_char buf '"';
    Buffer.add_string buf (String.escaped s); (* FIXME *)
    Buffer.add_char buf '"'

let make_authorize_response must_ok may_ok query_res =
  let buf = Buffer.create 80 in
  bprintf buf "{must_ok: %b, may_ok: [" must_ok;
  List.iteri
    (fun i group ->
      if i > 0 then Buffer.add_string buf ", ";
      Buffer.add_char buf '"';
      Buffer.add_string buf (String.escaped group);
      Buffer.add_char buf '"')
    may_ok;
  Buffer.add_char buf ']';
  List.iter
    (fun (an, vs) ->
      Buffer.add_string buf ", ";
      Buffer.add_string buf an;
      Buffer.add_string buf ": [";
      List.iteri
        (fun i v ->
          if i > 0 then Buffer.add_string buf ", ";
          bprint_json_value buf v)
        vs;
      Buffer.add_string buf "]")
    query_res;
  Buffer.add_string buf "}\n";
  Buffer.contents buf, "application/json"

let selected_entity s =
  Lwt.catch
    (fun () -> Entity.select_opt (selector_of_string s))
    (function
     | Failure msg | Invalid_argument msg -> http_error 400 msg
     | exn -> raise exn)

let restapi_service =
  let open Eliom_service in
  let get = Eliom_parameter.(
    string "subject" ** set string "must" ** set string "may" **
    set string "q")
  in
  create ~path:(Path ["restapi"; "authorize"]) ~meth:(Get get) ()

let _ =
  Eliom_registration.Any.register ~service:restapi_service
    @@ fun (subject, (must, (may, query))) () ->
  Lwt_log.debug_f
    "Checking %s against [%s] and optional [%s] groups"
    subject (String.concat ", " must) (String.concat ", " may) >>= fun () ->
  let* root = Entity.get_root () in
  let allowed_ans = Lazy.force allowed_attributes in
  let* must_ok, may_ok, query_res =
    (selected_entity subject >>= function
     | None -> Lwt.return (false, [], [])
     | Some user ->
        let is_member_of group =
          (selected_entity group >>= function
           | None -> Lwt.return false
           | Some group -> Entity.is_sub user group)
        in
        let get_attribute an =
          if not (String_set.mem an allowed_ans) then Lwt.return_none else
          Lwt.catch
            (fun () ->
              let* Attribute_type.Any at = Attribute_type.any_of_name_exn an in
              let vt = Attribute_type.value_type at in
              let+ vs = Entity.get_values at root user in
              let vs = Values.elements vs in
              Some (an, List.map (Value.to_json vt) vs))
            (function
             | Caqti_error.Exn _ -> Lwt.return_none
             | exn -> raise exn)
        in
        let* must_ok = Lwt_list.for_all_p is_member_of must in
        if must_ok then
          let* may_res = Lwt_list.filter_p is_member_of may in
          let+ query_res = Lwt_list.filter_map_p get_attribute query in
          (true, may_res, query_res)
        else
          Lwt.return (false, [], []))
  in
  let resp = make_authorize_response must_ok may_ok query_res in
  Eliom_registration.String.send (resp)
