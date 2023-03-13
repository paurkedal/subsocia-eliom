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

open Subsocia_selector_types

type ('p, 'a) path_template = {
  prefix: 'p;
  attribute: 'a;
  value_pattern: Re.re;
  value_template: string;
}

type condition =
  | Not of condition
  | And of condition list
  | Or of condition list
  | Has_header of string * Re.re
  | Has_remote_ip of Ipaddr.Prefix.t

val true_ : condition
val false_ : condition

type ('p, 'a) authentication_method =
  | Fixed of {
      identity: 'p;
    }
  | Trusted_header of {
      header: string;
      identity: ('p, 'a) path_template;
    }
  | Bearer_jwt of {
      jwk: Jose.Jwk.public Jose.Jwk.t;
      identity: ('p, 'a) path_template;
    }

type t = {
  restapi_allowed_attributes: string list;
  (** Attributes which are exposed by the REST API. This must be valid JSON
      identifiers due to the way they are returned. *)

  member_types: string list;
  (** A list of types of entities which can be assigned group membership via
      the web interface. If empty, all entities can be assigned membership. *)

  default_entity: string;
  (** Path to the entity to which the /entities URL redirects. *)

  completion_cutoff: float;
  (** Full-text search score cut-off to use in web interface. *)

  completion_limit: int;
  (** Maximum number of results to return for full-text search in web
      interface. *)

  change_log: string option;
  (** If set, the path to which to log Subsocia changes requested though the web
      interface.  A relative path is taken to be relative to the OCSIGEN log
      directory.  If unset, the default logs reporter will be used. *)

  authentication_rules: (condition * (selector, string) authentication_method) list;
  (** Rules for authenticating the remote user.  Only authentication methods
      paired with a successful condition will be tried, and the first method to
      succeed, if any, causes the whole chain to succeed. *)
}

val global : t
