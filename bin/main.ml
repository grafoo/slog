let ( >>= ) = Result.bind

let domain name =
  match Domain_name.of_string name with
  | Ok domain -> Ok domain
  | Error (`Msg e) -> Error e

let txt_one txt_set =
  match Dns.Rr_map.Txt_set.choose_opt txt_set with
  | Some txt -> Ok txt
  | None -> Error "No TXT record found"

let txt domain =
  let client = Dns_client_unix.create () in
  match Dns_client_unix.getaddrinfo client Dns.Rr_map.Txt domain with
  | Ok (_ttl, txt_set) -> txt_one txt_set
  | Error (`Msg e) -> Error e

let txts domain =
  let client = Dns_client_unix.create () in
  match Dns_client_unix.getaddrinfo client Dns.Rr_map.Txt domain with
  | Ok (_ttl, txt_set) -> Ok txt_set
  | Error (`Msg e) -> Error e

let unkv e = List.nth (String.split_on_char '=' e) 1

let index_filter_p s =
  if String.starts_with ~prefix:"i=" s || String.starts_with ~prefix:"n=" s then
    true
  else false

let concat_index_sub l =
  let name = List.find (String.starts_with ~prefix:"i=") l |> unkv in
  let next = List.find (String.starts_with ~prefix:"n=") l |> unkv in
  next ^ "." ^ name

let index_domain txt =
  try
    Ok
      (txt |> String.split_on_char ';' |> List.map String.trim
     |> List.filter index_filter_p |> concat_index_sub)
  with Failure e -> Error ("Parsing index failed with " ^ e)

let assemble txt_set =
  match Dns.Rr_map.Txt_set.elements txt_set with
  | [] -> Error "empty"
  | l -> (
      let result =
        List.map
          (fun s ->
            let i = String.index s ';' in
            let index = String.sub s 0 i in
            ( int_of_string index,
              String.sub s (i + 1) (String.length s - String.length index - 1)
            ))
          l
        |> List.sort (fun (i1, _s1) (i2, _s2) ->
               if Int.equal i1 i2 then 0 else if i1 > i2 then 1 else -1)
        |> List.fold_left (fun acc (_i, a) -> acc ^ a) ""
      in
      match Base64.decode result with Ok s -> Ok s | Error (`Msg e) -> Error e)

let lookup index post =
  if post != "" then
    match post |> domain >>= txts >>= assemble with
    | Ok s -> print_endline s
    | Error e -> print_endline e
  else
    match index |> domain >>= txt >>= index_domain with
    | Ok index_sub_domain -> index_sub_domain ^ "." ^ index |> print_endline
    | Error e -> print_endline e

let split_to_rec_len text index =
  if String.length text + String.length index < 256 then (text, "")
  else
    let sub_end = 255 - String.length index in
    ( String.sub text 0 sub_end,
      String.sub text sub_end (String.length index + String.length text - 255)
    )

let rec split text acc =
  match acc with
  | [] ->
      let index = "0;" in
      let sub, text = split_to_rec_len text index in
      split text [ (0, index ^ sub) ]
  | (i, t) :: tail ->
      let index = Printf.sprintf "%x;" (i + 1) in
      if String.length text < 256 then
        List.rev ((i + 1, index ^ text) :: (i, t) :: tail)
      else
        let sub, text = split_to_rec_len text index in
        split text ((i + 1, index ^ sub) :: (i, t) :: tail)

let print_fragments fragments =
  List.iter (fun (_i, s) -> print_endline s) fragments

let format file =
  let ic = open_in file in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  print_fragments (split (Base64.encode_string content) [])

open Cmdliner

let cmd_format =
  let info = Cmd.info "format" in
  let file = Arg.(value & opt file "" & info [ "f"; "file" ]) in
  Cmd.v info Term.(const format $ file)

let cmd_lookup =
  let info = Cmd.info "lookup" in
  let index =
    Arg.(value & opt string "_slog.yakbarber.org" & info [ "i"; "index" ])
  in
  let post = Arg.(value & opt string "" & info [ "p"; "post" ]) in
  Cmd.v info Term.(const lookup $ index $ post)

let cmd =
  let info = Cmd.info "slog" in
  Cmd.group info [ cmd_lookup; cmd_format ]

let () = exit (Cmd.eval cmd)
