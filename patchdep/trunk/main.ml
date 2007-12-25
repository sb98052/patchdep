(* Patchdep
 *
 * main.ml
 * Copyright (C) 2007-2008 
 * Sapan Bhatia <sapanb@cs.princeton.edu>
 * PlanetLab
 *)

open Unix
open Printf
open Types
open Helpers
open Dependencies

  (* Globals *)
let fname_patchset = ref ""
let fname_iwant = ref ""
let fname_dontwant = ref ""
let patch_level = ref 1
let dot = ref ""

let cmdspeclist =
  [
    ("-patchlevel",Arg.Set_int(patch_level),"\tPatch level");
    ("-patchset",Arg.Set_string(fname_patchset),"\tfile with the list of available patches in working order");
    ("-iwant",Arg.Set_string(fname_iwant), "\tfile with the list of patches you want");
    ("-dontwant",Arg.Set_string(fname_dontwant), "\tfile with the list of patches you want");
    ("-dot",Arg.Set_string(dot), "\toutput dependencies as a dot file");
  ]


let _ = 
  Arg.parse cmdspeclist (fun item -> ()) "Usage: patchdep <options>";  
  let patchset = read_list_from_file !fname_patchset in
  let iwant = read_list_from_file !fname_iwant in
  let dontwant = read_list_from_file !fname_dontwant in
    make_dep_map_file_lst !patch_level patchset;
    let visited = dep_dfs iwant in
    if (not (!dot= "")) then
      begin
        let fout = open_out !dot in
          dep_dot fout iwant;
          close_out fout
      end;
    dep_dfs_not visited dontwant
