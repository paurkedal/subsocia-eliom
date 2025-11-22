(* Copyright (C) 2015--2025  Petter A. Urkedal <paurkedal@gmail.com>
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

open Eliom_content.Html
open Lwt.Infix
open Lwt.Syntax
open Sociaweb_auth
open Sociaweb_request
open Sociaweb_services
open Subsocia_common
open Subsocia_connection
open Subsocia_selector

let registration_form (first_name, (last_name, email)) =
  [F.table ~a:[F.a_class ["assoc"]] [
    F.tr [
      F.th [F.txt "First name:"];
      F.td [F.Form.input ~input_type:`Text ~name:first_name F.Form.string];
    ];
    F.tr [
      F.th [F.txt "Last name:"];
      F.td [F.Form.input ~input_type:`Text ~name:last_name F.Form.string];
    ];
    F.tr [
      F.th [F.txt "Email:"];
      F.td [F.Form.input ~input_type:`Email ~name:email F.Form.string];
    ];
    F.tr [
      F.td [];
      F.td [F.Form.input ~input_type:`Submit ~value:"Register" F.Form.string];
    ];
  ]]

let () =
  Eliom_registration.Html.register ~service:registration_form_service
    @@ fun () () ->
  Lwt.return @@
    Eliom_tools.F.html
      ~title:"Registration"
      ~css:[["css"; "subsocia.css"]]
      (F.body [
        F.h1 [F.txt "Registration"];
        F.Form.post_form ~service:registration_post_service
                         registration_form ();
      ])

let () =
  Eliom_registration.Html.register ~service:registration_post_service
    @@ fun () (first_name, (last_name, email)) ->
  let* im =
    (get_authenticator_result () >>= function
     | Unregistered identity_material -> Lwt.return identity_material
     | Authenticated _ -> http_error 400 "Already registered."
     | Unauthenticated -> http_error 401 "Not authenticated."
     | Terminate msg -> http_error 500 msg)
  in
  let* at_first_name = Attribute_type.of_name_exn Type.String "first_name" in
  let* at_last_name = Attribute_type.of_name_exn Type.String "last_name" in
  let* at_email = Attribute_type.of_name_exn Type.String "email" in
  let* et_person = Entity_type.of_name_exn "person" in
  let* e_root = Entity.get_root () in
  let* e_new_user = Entity.create et_person in
  let* e_new_user_id = Entity.soid e_new_user in
  let* e_new_users =
    Entity.select_one (selector_of_string "/default/subsocia-autoregs")
  in
  let* () = Entity.force_dsub e_new_user e_new_users in
  let* () = Entity.set_value at_first_name first_name e_root e_new_user in
  let* () = Entity.set_value at_last_name  last_name  e_root e_new_user in
  let* () = Entity.set_value at_email      email      e_root e_new_user in
  let+ () = Entity.set_value im.attribute_type im.value im.source e_new_user in
  Eliom_tools.F.html
    ~title:"Welcome"
    ~css:[["css"; "subsocia.css"]]
    (F.body [
      F.h1 [F.txt "Welcome, "; F.txt first_name];
      F.p [
        F.a ~service:entities_service
          [F.txt "Your registration is complete."]
          (Some e_new_user_id);
      ];
    ])
