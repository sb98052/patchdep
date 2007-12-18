open Printf

let remove_parent_directory patchlevel str =
  let len = String.length str in
  let rec prune idx level =
    if (level=0) then
      String.sub str idx (len-idx-1) 
    else
      let i = try String.index_from str idx '/' with e->
        begin
        printf "Bad filename: %s\n" str;raise e
        end
      in
        prune (i+1) (level-1)
  in
    prune 0 patchlevel

let read_list_from_file f =
  if (f="") then []
  else
    let f_file = try open_in f with e -> printf "Could not open file
                                           %s\n" f;flush Pervasives.stdout;raise e
    in
    let rec read_file cur_list =
      let next_line = try Some(input_line f_file) with _ -> None in
        match next_line with
          | Some(inp_line) -> read_file (cur_list@[inp_line])
          | None -> close_in f_file;cur_list
    in
      read_file []
