(* Search interface for the dependency graph *)

open Types
open Printf

type dfs_state = Past | Present | Future


let dfs graph iterator start finish =
  let visited: (string,dfs_state) Hashtbl.t = Hashtbl.create 1024 in
  let rec dfs_visit start_fn finish_fn key neighbours =
    let adjlist_neighbours_itty (elt: string): unit =       
      let my_neighbours = try Hashtbl.find graph elt with Not_found->[] in
        dfs_visit start_fn finish_fn elt my_neighbours
    in
    let vornot = try Hashtbl.find visited key with Not_found->Future in
      if (vornot = Future) then
        begin
          Hashtbl.replace visited key Present;
          start_fn key neighbours;
          List.iter adjlist_neighbours_itty neighbours;
          finish_fn key neighbours;
          Hashtbl.replace visited key Past;       
        end
  in
    iterator (fun x y->(dfs_visit start finish x y)) graph;
    visited
