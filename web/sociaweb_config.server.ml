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

open Subsocia_selector_types

module Decode = Decoders_yojson.Basic.Decode

type ('p, 'a) identity_map = {
  source: 'p;
  attribute_type: 'a;
  value_pattern: Re.re;
  value_template: string;
}

type condition =
  | Not of condition
  | And of condition list
  | Or of condition list
  | Has_header of string * Re.re
  | Has_remote_ip of Ipaddr.Prefix.t

let true_ = And []
let false_ = Or []

type ('p, 'a) authentication_method =
  | Fixed of {
      identity: 'p;
    }
  | Trusted_header of {
      header: string;
      identity_map: ('p, 'a) identity_map;
    }
  | Trusted_environment of {
      variable: string;
      identity_map: ('p, 'a) identity_map;
    }
  | Bearer_jwt of {
      jwk: Jose.Jwk.public Jose.Jwk.t;
      identity_map: ('p, 'a) identity_map;
    }

type t = {
  restapi_allowed_attributes: string list;
  member_types: string list;
  default_entity: string;
  completion_cutoff: float;
  completion_limit: int;
  adjacency_limit: int;
  postpone_candidates: bool;
  change_log: string option;
  authentication_rules: (condition * (selector, string) authentication_method) list;
}

let re_decoder =
  let open Decode in
  let* s = string in
  try succeed (Re.compile (Re.Pcre.re s)) with
   | Re.Perl.Parse_error -> fail "invalid regular expression"
   | Re.Perl.Not_supported -> fail "unsupported regular expression"

let ipaddr_prefix_decoder =
  let open Decode in
  string
    >|= Ipaddr.Prefix.of_string
    >|= Result.map_error (function `Msg msg -> Decoders.Error.make msg)
    >>= from_result

let condition_decoder =
  let re_anything = Re.(compile bos) in
  let open Decode in
  fix @@ fun condition_decoder ->
  let* test = field "test" string in
  let* invert = field_opt_or ~default:false "invert" bool in
  let+ condition =
    (match test with
     | "satisfy-all" ->
        let+ conditions = field "conditions" (list condition_decoder) in
        And conditions
     | "satisfy-any" ->
        let+ conditions = field "conditions" (list condition_decoder) in
        Or conditions
     | "has-header" ->
        let* header = field "header" string in
        let+ pattern = field_opt_or ~default:re_anything "pattern" re_decoder in
        Has_header (header, pattern)
     | "has-remote-ip" ->
        let+ prefix = field "prefix" ipaddr_prefix_decoder in
        Has_remote_ip prefix
     | _ ->
        fail "Invalid test.")
  in
  if invert then Not condition else condition

let path_decoder =
  let open Decode in
  let* sel = string in
  try succeed (Subsocia_selector.selector_of_string sel) with
   | Invalid_argument msg -> fail msg

let identity_map_decoder =
  let default_value_pattern = Re.(compile (rep any)) in
  let default_value_template = "${0}" in
  let open Decode in
  let* source = field "source" path_decoder in
  let* attribute_type =
    field_opt_or ~default:"unique_name" "attribute_type" string in
  let* value_pattern =
    field_opt_or ~default:default_value_pattern "value_pattern" re_decoder in
  let+ value_template =
    field_opt_or ~default:default_value_template "value_template" string in
  {
    source;
    attribute_type;
    value_pattern;
    value_template;
  }

let trusted_header_decoder =
  let open Decode in
  let* header = field "header" string in
  let+ identity_map = field "identity_map" identity_map_decoder in
  Trusted_header {header; identity_map}

let trusted_environment_decoder =
  let open Decode in
  let* variable = field "variable" string in
  let+ identity_map = field "identity_map" identity_map_decoder in
  Trusted_environment {variable; identity_map}

let fixed_decoder =
  let open Decode in
  let+ identity = field "identity" path_decoder in
  Fixed {identity}

let jwk_decoder json =
  let conv_error = function
   | `Json_parse_failed msg -> Decoders.Error.make ("JSON parse error: " ^ msg)
   | `Msg msg -> Decoders.Error.make ("JWK parse error: " ^ msg)
   | `Unsupported_kty -> Decoders.Error.make "Unsupported JWK key type."
  in
  Jose.Jwk.of_pub_json (json : Yojson.Basic.t :> Yojson.Safe.t)
    |> Result.map_error conv_error

let bearer_jwt_decoder =
  let open Decode in
  let* jwk = field "jwk" jwk_decoder in
  let+ identity_map = field "identity_map" identity_map_decoder in
  Bearer_jwt {jwk; identity_map}

let authentication_rule_decoder =
  let open Decode in
  let* method_ = field "method" string in
  let* condition = field_opt_or ~default:true_ "when" condition_decoder in
  let+ authentication =
    (match method_ with
     | "trusted-header" -> trusted_header_decoder
     | "trusted-environment" -> trusted_environment_decoder
     | "fixed" -> fixed_decoder
     | "bearer-jwt" -> bearer_jwt_decoder
     | _ -> fail "Invalid authentication method.")
  in
  (condition, authentication)

let decoder =
  let open Decode in
  let* restapi_allowed_attributes =
    field "restapi_allowed_attributes" (list string) in
  let* member_types = field_opt_or ~default:[] "member_types" (list string) in
  let* default_entity = field_opt_or ~default:"/" "default_entity" string in
  let* completion_cutoff =
    field_opt_or ~default:0.001 "completion_cutoff" float in
  let* completion_limit = field_opt_or ~default:10 "completion_limit" int in
  let* adjacency_limit = field_opt_or ~default:25 "adjacency_limit" int in
  let* postpone_candidates =
    field_opt_or ~default:false "postpone_candidates" bool in
  let* change_log = field_opt "change_log" string in
  let+ authentication_rules =
    field "authentication_rules" (list authentication_rule_decoder) in
  {
    restapi_allowed_attributes;
    member_types;
    default_entity;
    completion_cutoff;
    completion_limit;
    adjacency_limit;
    postpone_candidates;
    change_log;
    authentication_rules;
  }

let global = Lwt_main.run begin
  let open Lwt.Syntax in
  let path =
    try Sys.getenv "SUBSOCIA_ELIOM_CONFIG" with
     | Not_found -> "/etc/subsocia-eliom.json"
  in
  let+ content = Lwt_io.with_file ~mode:Lwt_io.input path Lwt_io.read in
  (match Decode.decode_value decoder (Yojson.Basic.from_string content) with
   | Ok v -> v
   | Error msg ->
      Fmt.failwith "Cannot load %s: %a" path Decode.pp_error msg
   | exception Yojson.Json_error msg ->
      Fmt.failwith "Cannot load %s: %s" path msg)
end
