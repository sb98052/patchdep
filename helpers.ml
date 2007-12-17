open Printf

let read_list_from_file f =
  if (f="") then []
  else
  let f_file = try open_in f with e -> printf "Could not open file
                                         %s\n" f;flush Pervasives.stdout;raise e
  in
  let rec read_file cur_list =
    let next_line = try Some(input_line f_file) with _ -> None in
      match next_line with
        | Some(inp_line) -> read_file (inp_line::cur_list)
        | None -> close_in f_file;cur_list
  in
    read_file []
