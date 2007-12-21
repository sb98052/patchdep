(* Notes:
 *
 * - Since we only look at headers, ignoring the body of the patch, collision
 * resolution is approximate but conservative and sound.
 * TODO: 
 * -count maximum consecutive additions and deletions and store that 
 * to make this tool more accurate
 * -in the next step, keep track of exactly where the additions and deletions
 * are happening for precise dependency analysis
 *
 *)

open Pretty
open Printf
open Parser
open Lexer
open Types
open Search
open Helpers

type filename = string
type patchname = string

type commit = {
  st:int;
  num:int;
  id:int;
}
type stamp = patchname * commit ref

type depmap = ((patchname*patchname),(filename * stamp * stamp) list) Hashtbl.t
type changemap = (filename,stamp list) Hashtbl.t
type adjlist = (patchname,patchname list) Hashtbl.t

let change_map = Hashtbl.create 1024
let dep_map:depmap = Hashtbl.create 1024
let adj_list = Hashtbl.create 1024

let add_dependency file mypatch oldpatch myblock oldblock =
  (*printf "Adding dependency: {%s[%s:%d,%d (%d)]<- %s[%s:%d,%d (%d)]\n"
   oldpatch file !oldblock.st !oldblock.num !oldblock.id mypatch file
   !myblock.st !myblock.num !myblock.id;*)
  let cur_adj_list = try Hashtbl.find adj_list mypatch with Not_found->[] in
  if (not (List.exists (fun e->e=oldpatch) cur_adj_list)) then
    Hashtbl.replace adj_list mypatch (oldpatch::cur_adj_list);
  let cur_map_list = try Hashtbl.find dep_map (mypatch,oldpatch) with Not_found -> [] in
    begin
      let cur_map_list = (file, (mypatch,myblock),(oldpatch,oldblock))::cur_map_list in
        Hashtbl.replace dep_map (mypatch,oldpatch) cur_map_list
    end


(* The function that builds dependencies between patches. It performs the tasks
 * of: (i) rewriting
 * previous commits based on the current one (ii) adding edges in the dependency
 * graph.
 *
 * - Use start1 to find and update the old list
 * - Use start2 in the entries for the new commits AND don't bump this number
 * for the same patch. OR use start1 and treat commits as a flat list
 *
 * @param       patch                 Name of the patch that applies here
 * @param       file                  Name of the file in context
 * @param       commit_id             Position of commit in the series
 * @param       newcommit             Commit of type Changespec(...)
 * @param       cur_commitlist        List of previous commits
 *
 * returns (normalized new commit, normalized list of stamps)
 *)

let normalize_dependencies patch file commit_id newcommit cur_stamplist =
  (*TODO: check that we don't normalize entries in the current patch *) 
  let normalize arg_newcommit_o arg_newcommit_n displacement cur_stamplist oldstamp =
    let (oldpatch,oldcommit) = oldstamp in
    let (start1,count1)=(!arg_newcommit_o.st,!arg_newcommit_o.num) in
    let (start2,count2)=(!arg_newcommit_n.st,!arg_newcommit_n.num) in
    (*printf "Processing... %s:%s(%d) %d,%d,%d,%d\n" patch file commit_id
     * start1 count1 start2 count2;*)
    let (adddep,normalized_oldcommit) = 
      if (patch == oldpatch) then
        (false,oldcommit) 
        (* We come before this one *)
        else if (start1+count1 < !oldcommit.st) then
          begin
            oldcommit:={st = !oldcommit.st + displacement;num = !oldcommit.num;id= !oldcommit.id};
            (false, oldcommit)
              end
            (* We're right in the middle of this one - assume the worst forseeable
             * scenario (!)
             *)
            else if (start1< !oldcommit.st) then
              begin
                (* Careful - use start1 here because this item is going to be
                 * revisited and adjusted by other entries in thes patch*)
                if (start1+count1 < !oldcommit.st + !oldcommit.num) then
                  begin
                    oldcommit:= {st = start2;id= !oldcommit.id;num = !oldcommit.st+ !oldcommit.num-start1};
                    (true, oldcommit)
                  end
                else
                  begin
                    oldcommit:={st = start2;id= !oldcommit.id;num=count1};
                    (true,oldcommit)
                  end
              end
            (* we're somewhere in the middle of the old patch *)
            else if (start1< !oldcommit.st + !oldcommit.num)  then
              begin
                if (start1+count1 < !oldcommit.st + !oldcommit.num) then
                  begin
                    (* XXX BUG - displacement is not enough. eg. +++++++++++ X
                     * ---------- => at X the old patch just got bumped by all those
                     * +'s, but the displacement is 0 *)
                    oldcommit:={num = !oldcommit.num + displacement;id= !oldcommit.id;st = !oldcommit.st};
                    (true, oldcommit)
                  end
                else
                  begin
                    (* because !oldcommit.st doesn't cahnge *)
                    oldcommit:={num=start2+count2- !oldcommit.st;id= !oldcommit.id;st = !oldcommit.st};
                    (true, oldcommit)
                  end
              end
            else
              (false,oldcommit)
  in
    if (adddep) then
      add_dependency file patch oldpatch arg_newcommit_n normalized_oldcommit; 
    let normalized_stamp=(oldpatch,normalized_oldcommit) in
      (* :: is ok, since we're never going to apply a relation that
       * depends on the order of patches again *)
      cur_stamplist@[normalized_stamp]
in
  match newcommit with
    | ChangeSpec((start1,count1),(start2,count2)) -> 
        let c_newcommit_o =ref {st=start1;num=count1;id=commit_id}  in
let c_newcommit_n =ref {st=start2;num=count2;id=commit_id}  in
let normalized_stamp_lst = 
  List.fold_left (normalize c_newcommit_o c_newcommit_n (count2-count1)) [] cur_stamplist in
  (* The commits are in -ascending- chronological order. We need to
   * correct them explicitly *)
  (*printf "Start2=%d,count2=%d\n" start2 count2;*)
  ((patch,c_newcommit_n),normalized_stamp_lst)


let accept_commits patch patchlevel fs =
  match fs with
    | FileSpec(str_local, commitlist) ->
        let str = remove_parent_directory patchlevel str_local in
let cur_stamplist = try Hashtbl.find change_map str with Not_found->([]) in
let calc_stamp arg newcommit =
  let (counter,arg_cur_stamplist) = arg in
let (stamp0,arg_cur_stamplist0) = normalize_dependencies patch str counter newcommit arg_cur_stamplist in
  (counter+1,arg_cur_stamplist0@[stamp0])
in
let (_,new_stamplist) = List.fold_left calc_stamp (0,cur_stamplist) commitlist in
  Hashtbl.replace change_map str new_stamplist

let make_dep_map_file_lst patchlevel patch_file_list =
  let make_dep_map fname = 
    let fin = 
      try open_in fname 
            with e -> 
              printf "Could not open file %s.\n" fname;
              flush Pervasives.stdout;raise e
                                        in
let lexbuf = Lexing.from_channel fin in
let result = Parser.file Lexer.scanner lexbuf in
  List.iter (accept_commits fname patchlevel) result;
             close_in fin
               in
  List.iter make_dep_map patch_file_list

let dep_dfs good_lst =
  let itty payload ht =
    let my_itty x y =
      if (List.exists (fun a->(a=x)) good_lst) then
        begin
          payload x y
            end
in
  Hashtbl.iter my_itty ht
    in
let need_this_patch x y = printf "%s\n" x;flush Pervasives.stdout in
  printf "Required patches:\n";
  dfs adj_list itty need_this_patch (fun x y ->())

let dep_dot fout good_lst =
  let itty payload ht =
    let my_itty x y =
      if (List.exists (fun a->(a=x)) good_lst) then
        begin
          payload x y
        end
    in
      Hashtbl.iter my_itty ht
  in
    fprintf fout "/* Generated by patchdep */\n";
    fprintf fout "digraph dotty {\n";
    let print_dot_dep x y =
      let print_deps s d =
        fprintf fout "\"%s\"->\"%s\"\n" s d in
        List.iter (print_deps x) y 
    in
      ignore (dfs adj_list itty print_dot_dep (fun x y ->()));
      fprintf fout "}\n"

let dep_dfs_not visited bad_lst =
  let itty payload ht =
    let my_itty x y = 
      if (Hashtbl.mem visited x) then
        payload x y
    in
      Hashtbl.iter my_itty ht
  in
  let print_deps key neigh =
    let print_deps_pair_if src dst =
      if (List.exists (fun a->(a=dst)) bad_lst) then
        let print_deps_pair src dst =
          let deps = Hashtbl.find dep_map (src,dst) in
          let print_dep (dep:(filename*stamp*stamp)) = 
            let (fname, (patch1,commit1), (patch2,commit2)) = dep in
              printf "%s --> %s[%d],%s[%d]\n" fname patch1 (!commit1.id) patch2 (!commit2.id)
          in
            List.iter print_dep deps
        in
          print_deps_pair src dst
    in
      List.iter (print_deps_pair_if key) neigh
  in
    printf "Conflicts to be resolved (<filename> --> <patch>[position of commit] <patch>[position of commit]:\n";
    ignore (dfs adj_list itty print_deps (fun x y -> ()))
