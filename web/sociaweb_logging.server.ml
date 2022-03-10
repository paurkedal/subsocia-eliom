(* Copyright (C) 2019--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let buf_fmt () =
  let buf = Buffer.create 512 in
  let ppf = Fmt.with_buffer buf in
  let flush () = let m = Buffer.contents buf in Buffer.reset buf; m in
  (ppf, flush)

let pp_ptime =
  Ptime.pp_human ?tz_offset_s:(Ptime_clock.current_tz_offset_s ()) ()

let channel_reporter oc_thread =
  let app, app_flush = buf_fmt () in
  let dst, dst_flush = buf_fmt () in
  let pp_header (ppf : Format.formatter) _ =
    let t = Ptime_clock.now () in
    Fmt.(const string "[" ++ pp_ptime ++ const string "] ") ppf t
  in
  let buf_reporter = Logs.format_reporter ~pp_header ~app ~dst () in
  let report src level ~over k msgf =
    let k () =
      let write () =
        let%lwt oc = oc_thread in
        (match level with
         | Logs.App -> Lwt_io.write oc (app_flush ())
         | _ ->        Lwt_io.write oc (dst_flush ())) >>= fun () ->
        Lwt_io.flush oc
      in
      let unblock () = over (); Lwt.return_unit in
      Lwt.async (fun () -> Lwt.finalize write unblock);
      k ()
    in
    buf_reporter.Logs.report src level ~over:(fun () -> ()) k msgf
  in
  {Logs.report}

let change_src =
  let doc = "Subsocia changes from the subsocia-eliom web interface." in
  Logs.Src.create ~doc "subsocia-eliom.change"

module Change_log = (val Logs_lwt.src_log change_src)

let () =
  Logs.Src.set_level change_src (Some Logs.Info);
  (match Sociaweb_config.change_log#get with
   | None -> ()
   | Some fp' ->
      let fp =
        if not (Filename.is_relative fp') then fp' else
        let logdir = Ocsigen_config.get_logdir () in
        Filename.concat logdir fp'
      in
      let oc = Lwt_io.open_file
        ~mode:Lwt_io.output
        ~flags:Unix.[O_CREAT; O_WRONLY; O_APPEND; O_NONBLOCK] fp
      in
      let their_reporter = Logs.reporter () in
      let our_reporter = channel_reporter oc in
      let report src level ~over k msgf =
        let reporter =
          if Logs.Src.equal src change_src then our_reporter else their_reporter
        in
        reporter.Logs.report src level ~over k msgf
      in
      Logs.set_reporter {Logs.report})
