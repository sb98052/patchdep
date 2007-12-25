(* Patchdep
 *
 * types.ml
 * Copyright (C) 2007-2008 
 * Sapan Bhatia <sapanb@cs.princeton.edu>
 * PlanetLab 
 * 
 *)

type linespec = int * int
type changespec = ChangeSpec of linespec * linespec
type filespec = FileSpec of string * changespec list
type filespeclist = filespec list
