(* Patchdep
 *
 * dependency.ml
 * Copyright (C) 2007-2008 
 * Sapan Bhatia <sapanb@cs.princeton.edu>
 * PlanetLab
 *)

open Types
open Printf

let print_change_spec cs =
  match cs with
    | ChangeSpec(lsin,lsout) ->
        let min,nin = lsin in
        let mout,nout = lsout in
          printf "-%d,%d +%d,%d\n" min nin mout nout

let print_file_spec fs = 
  match fs with
    | FileSpec(str, cslst) ->
        printf "File: %s\n" str;
        List.iter print_change_spec cslst

