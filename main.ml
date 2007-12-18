open Unix
open Printf
open Types
open Helpers
open Dependencies

  (* Globals *)
let fname_patchset = ref ""
let fname_iwant = ref ""
let fname_dontwant = ref ""

let cmdspeclist =
  [
    ("-patchset",Arg.Set_string(fname_patchset),"\tfile with the list of available patches in working order");
    ("-iwant",Arg.Set_string(fname_iwant), "\tfile with the list of patches you want");
    ("-dontwant",Arg.Set_string(fname_iwant), "\tfile with the list of patches you want");
  ]


let _ = 
  Arg.parse cmdspeclist (fun item -> ()) "Usage: patchdep <options>";  
  let patchset = read_list_from_file !fname_patchset in
  let iwant = read_list_from_file !fname_iwant in
  let dontwant = read_list_from_file !fname_dontwant in
    make_dep_map_file_lst patchset
