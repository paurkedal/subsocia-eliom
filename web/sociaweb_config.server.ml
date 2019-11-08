(* Copyright (C) 2015--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

let group = new Config_file.group

let auth_top =
  new Config_file.string_cp ~group
    ["auth_top"] "auth"
    "Group under which authentication methods are placed."
let hba_method_header =
  new Config_file.option_cp Config_file.string_wrappers ~group
    ["hba_method_header"] None
    "HTTP header containing the authentication method if not fixed."
let hba_method =
  new Config_file.option_cp Config_file.string_wrappers ~group
    ["hba_method"] (Some "default")
    "Authentication method if hba_method_header is unset or not the \
     header is missing."
let hba_identity_header =
  new Config_file.option_cp Config_file.string_wrappers ~group
    ["hba_identity_header"] (Some "Host")
    "HTTP header containing the identity of an authenticated user. \
     If unset, disables header-based authentication."
let restapi_allowed_attributes =
  new Config_file.list_cp Config_file.string_wrappers ~group
    ["restapi_allowed_attributes"] []
    "Attributes which are exposed by the REST API. This must be \
     valid JSON identifiers due to the way they are returned."

let member_types =
  new Config_file.list_cp Config_file.string_wrappers ~group
    ["member_types"] ["person"]
    "A list of types of entities which can be assigned group membership via \
     the web interface. If empty, all entities can be assigned membership."

let default_entity =
  new Config_file.string_cp ~group
    ["default_entity"] "/"
    "Path to the entity to which the /entities URL redirects."

let completion_cutoff =
  new Config_file.float_cp ~group
    ["completion_cutoff"] 0.001
    "Full-text search score cut-off to use in web interface."
let completion_limit =
  new Config_file.int_cp ~group
    ["completion_limit"] 10
    "Maximum number of results to return for full-text search in web \
     interface."

let change_log =
  new Config_file.option_cp Config_file.string_wrappers ~group
    ["change_log"] None
    "If set, the path to which to log Subsocia changes requested though the \
     web interface. \
     A relative path is taken to be relative to the OCSIGEN log directory. \
     If unset, the default logs reporter will be used."

let () =
  try group#read (Sys.getenv "SUBSOCIA_ELIOM_CONFIG")
  with Not_found ->
    if Sys.file_exists "/etc/subsocia-eliom.conf" then
      group#read "/etc/subsocia-eliom.conf"
