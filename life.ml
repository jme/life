(* This is the "hybrid" version of the OCaml-based life game.
 
  Whereas the previous version used an Int-keyed map this version uses a plain Array
  along with a mutable state field in the cellrec record.
 
  The raw array references are quite a bit faster than the Map lookups.
 
  Note that a fresh Array (representing the game grid) is generated for each
  generation, just as in the case of the previous Map-based version.
 
  Another level of optimization (of at least, GC workload) might be available if that Array was just mutated
  instead of regenerated. But this was an excercise in keeping to the
  functional side of OCaml... 
  
  Basically stream-of-consciousness coding to find my way around OCaml*)

open Graphics;;


(* the data for each cell is held in records (cellrecs).
  The v, row and col items contain positional and state values.
  Access keys (just, sequence numbers  of int) for neighboring cells are collected into the in list item *)  

type cellrec = {n: int list; mutable v: bool; row: int; col: int;}



(* some dev helper functions: ---------------------- *)

let box_printer key xrec = 
  print_string("\nbox printer\n");
  print_string("key: " ^ (string_of_int key) ^  ", row: " ^ (string_of_int xrec.row) ^ ", col: " ^ (string_of_int xrec.col) ^ ", v: " ^ (string_of_bool xrec.v ^ "\n") ^ "# of neighbors: " ^ (string_of_int (List.length xrec.n)));;


let print_ints k = 
  print_string("\nint: " ^ (string_of_int k) ^  "\n") ;;




(* Yeah sure, I could have used List.map but I just wanted to use pattern-matching
  some more. Is that wrong?    *)

let rec map f xs =
  match xs with
    | []  -> []
    | hd::tl -> f hd :: map f tl
;; 



(* these are the cell coordinate transformer functions for the neighboring cell
  calculations. They are used in pair-combinations,  one for row and one for col.  *)

let xm1 a = a - 1 ;;
let xp1 a = a + 1 ;;
let xid a = a;;



(* Assemble the previous coordinate functions into a list covering each of the 8
  theoretical neighbors of a given cell.
  Because of frame boundaries not all of these will actually be possible/used, for a given cell  *)

let xfuns = 
  [ (xm1, xp1);
    (xid, xp1);
    (xp1, xp1);
    (xm1, xid);
    (xm1, xm1);
    (xid, xm1);
    (xp1, xm1);
    (xp1, xid)] ;;



(* get random number generator wound up *)
Random.self_init () ;;



(* does what it says on the tin *)

let inBounds (a : int)  (low_bound : int)  (high_bound : int ) = 
  (a > low_bound) && (a < high_bound) ;;




(* Want to calculate the actual neighbors of a given cell.
  Fortunately such neighbors are easily characterized by a few simple
  arithmetic expressions. The next two functions do this:
 
  This first function applies row & col transformer expressions to a row & col,
  and, returns the sequence number of the cell. Fail -> 0; *) 

let  getSN  span  row  col  fa  fb =

  let new_row = fa row in
  let new_col = fb col in
   
    if (inBounds new_row 0 (span + 1)) && (inBounds new_col 0 (span + 1)) 
    then new_row + (new_col - 1) * span 
    else 0 ;;



(* Now this second function tallies only the valid neighbors of a given cell *)

let getNeighbors  row  col  span  crits =

  (List.filter (fun a -> a > 0) 
               (map 
                   (fun k -> getSN span row col (fst k) (snd k))
                    crits )) ;;



(* 
 Here is where the intial grid is assembled.
 Cells are set with random state, their neighbors noted, etc.

 This function prepares and returns a context-tuned grid assembler that does the
 actual work.  *)

let grid_assembler aspan neighbor_fns= 

  let ws = Array.make ((aspan + 1) * (aspan + 1)) {n=[]; v=false; row=0; col=0} in
  let rec assembler nrow ncol ticks =

    if nrow = aspan then
        begin
          if ncol <> aspan then  
           begin 
            ws.(ticks) <- {n=(getNeighbors 1 (ncol+1) aspan neighbor_fns); v=(Random.bool ()); row=1; col=(ncol+1)}; 
            assembler  1 (ncol + 1) (ticks + 1)
           end
        end

    else 
      begin
        ws.(ticks) <- {n=(getNeighbors (nrow+1) ncol aspan neighbor_fns); v=(Random.bool ()); row=(nrow+1); col=ncol};
        assembler (nrow+1) ncol (ticks+1)
      end
  in
  
  assembler 0 1 1;
  ws
;;





(* The core metric in Life is the count of the live 'neighbor' cells around a
  given cell. 
 
  In the cellrec there is a list of neighbor cells, pre-filled as a part of
  program initialization.
 
  Given this list of sequence numbers, scan through, looking up the cellrec
  associated with each of sequence number.
 
  if that cellrec has a LIVE status, accumulate the sequence number into a list.
 
  Finally, return the COUNT of accumulated list of all sequence numbers of cells that were LIVE.  *)

let getLiveCount  ws  (seq_list: int list) =

  let rec seqscan xs result =
    match xs with
      |  [] -> result
      |  hd::tl ->  if (ws.(hd)).v && true then seqscan tl (hd :: result) else seqscan tl result;
  in
    List.length (seqscan seq_list [] ) ;;




(* this looks at an existing cellrec and determines its next state 
 
  Note: those last couple if true-case match criteria could be consolidated but are kept discrete at
  the moment in order to help interpret profiling stats    *)

let getNewState   ws   neighbors  current_state  = 

  let live_count = ((getLiveCount ws neighbors) : int)  in

    match current_state with
      |  false -> begin
                    match live_count with
                      |  3 -> true
                      | _  -> false
                  end 
      
      |  true -> begin
                   match live_count with
                     |  3 -> true
                     |  2 -> true
                     |  1 -> false
                     |  0 -> false
                     | _  -> false
                 end
;;

     


(* yeah, I know.
  But the run_env ref is only mutated (@ startup) in order to accumulate the environment
  described by the command line params.
  This reduces some function chain complexity and gets the job done. So there's that. *)

type run_state_description = {zoom : int; 
                              mspan : int; 
                              padding : int; 
                              bkcolor : color; 
                              fgcolor : color};;

(* pre-fill the mutable with a default environment in case nothing is specified by the command
  line params. *)
let  run_env = ref 
                 {zoom = 3; 
                  mspan = 200; 
                  padding = 5; 
                  bkcolor = (rgb 253 246 227); 
                  fgcolor = (rgb 130 130 130)};;



(* themes are accessed via this associative container *)
module ThemeRepo = Map.Make(String);;

(* themes are currently just a matter of fore and background colors *)
type theme_record = {bcolor : color; fcolor : color};;
 
(* reference list of pre-baked, theme data. Not currently possible for users to add themes *)
let current_themes = 
                   [("cosmic", {bcolor = (rgb 4 4 4); fcolor = (rgb 10 10 180)});
                    ("amberish", {bcolor = (rgb 10 10 10 ); fcolor = (rgb 204 203 105)});
                    ("oldskool", {bcolor = (rgb 253 246 227); fcolor = (rgb 130 130 130)});
                    ("moo", {bcolor = (rgb 4 4 4); fcolor = (rgb 0 170 0)})  ] ;;




(* It is convenient to store the theme data in a map, using the theme name as
  the key.  This way you can quickly reference individual cell records   
  So, first fill a lookup map with theme info from the referenc list   *)

let rec mapfiller xs box =
  match xs with
    | [] -> box
    | hd::tl -> mapfiller  tl (ThemeRepo.add (fst hd) (snd hd) box) ;;



(* and then here is the filled theme map value   *)

let themes = mapfiller  current_themes  ThemeRepo.empty;;



(* dev *)
let theme_printer key xrec = 
  print_string("\nkey: " ^  key) ;;

(* ThemeRepo.iter theme_printer themes;; *)



(* returns a cell rendering function, with the current game environment baked-in  *)

let getFillPlotter env =

  (fun xrec -> begin
                   if xrec.v = true then set_color env.fgcolor else set_color env.bkcolor;
                   
                   fill_rect (env.padding + (env.zoom * (xrec.row - 1))) 
                             (env.padding + ( env.zoom * (xrec.col - 1 ))) 
                             env.zoom 
                             env.zoom  
                  end) ;;




(* flush the game grid with the appropriate background *)

let clear_the_dancefloor env = 

    set_color env.bkcolor;
    fill_rect 0 0 ( (2 * env.padding) + (env.zoom * env.mspan)) 
                  ( (2 * env.padding) + (env.zoom * env.mspan)) ;;
 



(* Here is where the life algo rubber hits the road. 
   The 'reaper' is a function that is mapped over the work_set and which returns
   a cellrec containing the new cell state.   *)

let getReaper xs = 
  (fun c -> {n=c.n; v=(getNewState xs c.n c.v); row=c.row; col=c.col;}) ;;



(* this runner fn is where the bulk of the  work is done    *)

let runner reps xs =

  let env = !run_env in
  let renderer = (getFillPlotter env) in 
  
  (* note that the  in-cycle map function is returning a fresh Array each cycle.
    Not as efficient as in-place mutation of a single arry, to be sure.
    But this hybrid approach is simply to replace IntMap or Hashtbl lookups
    with much faster direct array index references.   *)
  let rec looper ticks msrc =
    Array.iter renderer msrc;  
    synchronize ();

    if ( key_pressed () = false) && (ticks < reps) then looper (ticks + 1) (Array.map (getReaper msrc) msrc)
                   else ticks in 

  looper 0 xs ;;



(* make ready a graphic surface, with padding. *)

let init_graphics env =
  let gridspan = ( (2 * env.padding) + (env.zoom * env.mspan)) in

    open_graph (" " ^ (string_of_int gridspan) ^ "x" ^ (string_of_int gridspan));
    auto_synchronize false;
    set_window_title "life.ml";
    clear_the_dancefloor env ;
    at_exit (fun () -> print_string "\n-------> Life.eol \n"; close_graph ());;



(* all of these environment setter functions are called by the Arg handler. There's an abstraction hiding around here somewhere... *)

let set_span n =
  print_string("\nspan: " ^ (string_of_int n));
  run_env := {zoom = !run_env.zoom; mspan = n; padding = !run_env.padding; bkcolor = !run_env.bkcolor; fgcolor = !run_env.fgcolor} ;;

let set_zoom n =
  print_string(", zoom: " ^ (string_of_int n));
  run_env := {zoom = n; mspan = !run_env.mspan; padding = !run_env.padding; bkcolor = !run_env.bkcolor; fgcolor = !run_env.fgcolor} ;;

let set_theme x  =
  let t_rec = ThemeRepo.find x themes in
    run_env :=  {zoom = !run_env.zoom; mspan = !run_env.mspan; padding = !run_env.padding; bkcolor = t_rec.bcolor; fgcolor = t_rec.fcolor} ;;



(* returns an Arg spec function with pre-baked span and zoom values   *)

let get_mode_fn z m =
  (fun () ->  run_env := {zoom = z; mspan = m; padding = !run_env.padding; bkcolor = !run_env.bkcolor; fgcolor = !run_env.fgcolor}) 
;;



(* command line specification, using the standard lib Arg module    *)

let speclist = [( "-regular", Arg.Unit (get_mode_fn 3 200), "(preset, with span: 200 and zoom: 3)\n");
                ( "-rapido", Arg.Unit  (get_mode_fn 6 120), "(preset, with span: 120 and zoom: 6)\n");
                ( "-medium", Arg.Unit (get_mode_fn 3 300), "(preset, with span: 300 and zoom: 2)\n");
                ( "-jumbo", Arg.Unit (get_mode_fn 2 600), "(preset, with span: 600 and zoom: 2)\n");
                ( "-mega", Arg.Unit (get_mode_fn 1 1000), "(preset, with span: 1000 and zoom: 1)\n");
                ( "-t", Arg.String (set_theme), "use a preset color theme.  Options: [ cosmic, moo, oldskool, bw, amberish]\n");
                ( "-s", Arg.Int (set_span), "set custom span size. \n");
                ( "-z", Arg.Int (set_zoom), "set custom zoom value  \n"); ] ;;



(* help text is assembled here    *)

let get_help_text  =

   let helpframe_top =
    "\n********************************************************************************
    " in

   let helpinfo = "\nConway's Life game. \n\nIn this famous 'zero player' game cells in a grid live or die depending upon" in

   let helpinfo2 = "\nthe states of surrounding cells. This simple algorithm produces a staggering" in
  
 
   let helpinfo3 = "\nvariety of patterns and behaviors. \n\nYou may choose your own values for the size of the grid (span) \nand the size of individual cells (zoom), or just use one of the provided presets.\n\nYou can also choose one of the preset color schemes.\n " in

   let help_example1 =
    "\n-------\n example 1: use the 'rapido' configuration, along with the 'moo' theme\n ./life -rapido -t moo\n " in

   let help_example2 =
    "\n example 2: use a custom span and zoom, with the default color scheme \n./life -s 180 -z 2 \n" in

   let help_example3 =
    "\n example 3: just run it, using all default values \n ./life \n" in

   helpframe_top ^ helpinfo  ^ helpinfo2 ^ helpinfo3 ^ help_example1 ^ help_example2 ^ help_example3 ^ "\n-------\n Usage and Options: "
;;


(* main app entry point, complete with a somewhat smelly way of handling help text. *)

let () =
  Arg.parse speclist (fun a -> ()) (get_help_text) ;

  init_graphics !run_env;
 
  let grid_array = grid_assembler !run_env.mspan xfuns in

  let start_ts = Sys.time() in
  let completed_reps = (runner max_int grid_array) in
  let end_ts = Sys.time() in
    print_string("\n\n generations: " ^ string_of_int completed_reps);
    print_string( Printf.sprintf "\n total runtime: %.2f" (end_ts -. start_ts));
    print_string( Printf.sprintf "\n FPS:  %.2f" ( (float_of_int completed_reps) /.  (end_ts -.  start_ts)))
;;

 read_line  ();;


