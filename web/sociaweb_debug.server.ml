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

open Eliom_content.Html

let debug_service =
  let open Eliom_service in
  create ~path:(Path ["debug"; "http_headers"])
         ~meth:(Get Eliom_parameter.unit) ()

let _ =
  Eliom_registration.Html.register ~service:debug_service
    (fun () () ->
      let headers = Eliom_request_info.get_ri ()
        |> Ocsigen_request.to_cohttp
        |> Cohttp.Request.headers
      in
      let tr_of_header name s = F.tr [F.td [F.txt name]; F.td [F.txt s]] in
      Lwt.return @@ Eliom_tools.F.html ~title:"HTTP Headers" @@ F.body [
        F.table ~a:[F.a_class ["std"]]
          (List.rev
            (Cohttp.Header.fold
              (fun name vs acc -> tr_of_header name vs :: acc)
              headers []))
      ])
