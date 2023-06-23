(* Copyright (C) 2023  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Eliom Services *)

[%%shared.start]

val entities_service :
  (int32 option, unit, Eliom_service.get, Eliom_service.att,
   Eliom_service.non_co, Eliom_service.non_ext, Eliom_service.reg,
   [ `WithSuffix ], [ `One of int32 ] Eliom_parameter.param_name,
   unit, Eliom_service.non_ocaml)
  Eliom_service.t

[%%server.start]

val entities_self_service :
  (unit, unit, Eliom_service.get, Eliom_service.att,
   Eliom_service.non_co, Eliom_service.non_ext, Eliom_service.reg,
   [ `WithoutSuffix ], unit, unit, Eliom_service.non_ocaml)
  Eliom_service.t

val registration_form_service :
  (unit, unit, Eliom_service.get, Eliom_service.att,
   Eliom_service.non_co, Eliom_service.non_ext, Eliom_service.reg,
   [ `WithoutSuffix ], unit, unit, Eliom_service.non_ocaml)
  Eliom_service.t

val registration_post_service :
  (unit, string * (string * string), Eliom_service.post,
   Eliom_service.att, Eliom_service.co, Eliom_service.non_ext,
   Eliom_service.reg, [ `WithoutSuffix ], unit,
   [ `One of string ] Eliom_parameter.param_name *
   ([ `One of string ] Eliom_parameter.param_name *
    [ `One of string ] Eliom_parameter.param_name),
   Eliom_service.non_ocaml)
  Eliom_service.t
