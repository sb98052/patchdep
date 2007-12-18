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

let change_map = Hashtbl.create 1024
let dep_map = Hashtbl.create 1024

let add_dependency file mypatch oldpatch myblock oldblock =
  printf "Adding dependency %s->%s (%d,%d) [%s:%d] [%d]\n" mypatch oldpatch !oldblock.st !oldblock.num file !oldblock.id !myblock.id;flush Pervasives.stdout;
  let cur_adj_list = try Hashtbl.find dep_map (mypatch,oldpatch) with Not_found -> (ref []) in
    begin
      cur_adj_list := (myblock,oldblock)::!cur_adj_list;
      Hashtbl.replace dep_map (mypatch,oldpatch) cur_adj_list
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
  let normalize arg_newcommit displacement cur_stamplist oldstamp =
    let (oldpatch,oldcommit) = oldstamp in
    let (start1,count1)=(!arg_newcommit.st,!arg_newcommit.num) in
    let (adddep,normalized_oldcommit) = 
      (* We come before this one *)
      if (start1+count1 < !oldcommit.st) then
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
              oldcommit:={st = start1;id= !oldcommit.id;num = !oldcommit.st+ !oldcommit.num-start1};
              (true, oldcommit)
            end
          else
            begin
              oldcommit:={st = start1;id= !oldcommit.id;num=count1};
              (true,oldcommit)
            end
        end
      (* we're somewhere in the middle of the old patch *)
      else if (start1< !oldcommit.st + !oldcommit.num)  then
        begin
          if (start1+count1 < !oldcommit.st + !oldcommit.num) then
            begin
              oldcommit:={num = !oldcommit.num + displacement;id= !oldcommit.id;st = !oldcommit.st};
              (true, oldcommit)
            end
          else
            begin
              oldcommit:={num=start1+count1- !oldcommit.st;id= !oldcommit.id;st = !oldcommit.st};
              (true, oldcommit)
            end
        end
      else
        (false,oldcommit)
    in
      if (adddep) then
        add_dependency file patch oldpatch arg_newcommit normalized_oldcommit; 
      let normalized_stamp=(oldpatch,normalized_oldcommit) in
        (* :: is ok, since we're never going to apply a relation that
         * depends on the order of patches again *)
        normalized_stamp::cur_stamplist
  in
    match newcommit with
      (* We ignore start2 and count2 because they get calculated implicitly *) 
      | ChangeSpec((start1,count1),(_,count2)) -> 
          let c_newcommit =ref {st=start1;num=count1;id=commit_id}  in
          let normalized_stamp_lst = 
            List.fold_left (normalize c_newcommit (count2-count1)) [] cur_stamplist in
            ((patch,c_newcommit),normalized_stamp_lst)


let accept_commits patch fs =
  match fs with
    | FileSpec(str, commitlist) ->
        let cur_stamplist = try Hashtbl.find change_map str with Not_found->([]) in
        let calc_stamp arg newcommit =
          let (counter,arg_cur_stamplist) = arg in
          let (stamp0,arg_cur_stamplist0) = normalize_dependencies patch str counter newcommit arg_cur_stamplist in
            (counter+1,stamp0::arg_cur_stamplist0)
        in
        let (_,new_stamplist) = List.fold_left calc_stamp (0,cur_stamplist) commitlist in
          Hashtbl.replace change_map str new_stamplist

let make_dep_map_file_lst patch_file_list =
  let make_dep_map fname = 
    let fin = 
      try open_in fname 
      with e -> 
        printf "Could not open file %s.\n" fname;
        flush Pervasives.stdout;raise e
    in
    let lexbuf = Lexing.from_channel fin in
    let result = Parser.file Lexer.scanner lexbuf in
      List.iter (accept_commits fname) result
  in
    List.iter make_dep_map patch_file_list
