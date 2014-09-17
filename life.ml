(* This is my first version of the OCaml-based life game.
   
   Life isI tried to keep to the functional side of OCaml 
 
  It uses an Int-keyed map to hold the cellrecs record.
 
  The performance of this is thus fairly strongly tied to the speed of those map
  lookups. (O log N) for the balanced binary tree-backed std lib Map.

  Note that a fresh Map (representing the game grid) is generated for each
  generation.
  
  Basically stream-of-consciousness coding to find my way around OCaml*)

open Graphics;;



(* Need an int-keyed map structure to hold all of the various cells that make up
  the Life game environment.
 
  The key used will be a cell sequence number created on game initialization.  *)

module IntMap = Map.Make( struct
  type t = int
  let compare = compare
end)



(* the data for each cell is held in records (cellrecs).
  The v, row and col items contain positional and state values.
  Access keys (just, sequence numbers  of int) for neighboring cells are
  collected into the in list item    *)  

type cellrec = {n: int list; v: bool; row: int; col: int;}



(* some dev helper functions *)

let box_printer key xrec = 
  print_string("\nbox printer\n");
  print_string("key: " ^ (string_of_int key) ^  ", row: " ^ (string_of_int xrec.row) ^ ", col: " ^ (string_of_int xrec.col) ^ ", v: " ^ (string_of_bool xrec.v ^ "\n") ^ "# of neighbors: " ^ (string_of_int (List.length xrec.n)));;


let print_ints k = 
  print_string("\nint: " ^ (string_of_int k) ^  "\n") ;;


(* dump IntMap data records *)

let show_cell_recs rec_tuple =
  let xrec = 
    fst rec_tuple in
    print_string("\nREC.row: " ^ string_of_int xrec.row ^ "\n");
    let xlst =
      snd rec_tuple in
      IntMap.iter box_printer xlst ;;

(* --------------------------------------------------*) 



(* Yeah sure, I could have used the existing List.map but I just wanted to
  excercise pattern matching a bit more.   *)

let rec map f xs =
  match xs with
    | [] -> []
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


(* Now this second function tallies only the valid neighbors of a given cell   *)

let getNeighbors  row  col  span  crits =

  (List.filter (fun a -> a > 0) 
               (map 
                   (fun k -> getSN span row col (fst k) (snd k))
                    crits )) ;;




(* Here is where the intial grid is assembled.
 Cells are set with random state, their neighbors noted, etc.

 This function prepares and returns a context-tuned grid assembler that does the
 actual work.

 And The output of that HOF is an IntMap with the cell sequence number as an int key,
 and having a celllrec value.  *)

let get_grid_assembler aspan neighbor_fns= 

  let rec assembler box nrow ncol ticks =

    if nrow = aspan then
        begin
          if ncol = aspan then box

          else assembler 
                      (IntMap.add ticks 
                                  {n=(getNeighbors 1 (ncol+1) aspan neighbor_fns); v=(Random.bool ()); row=1; col=(ncol+1)} 
                                  box) 
                      1 (ncol + 1) (ticks + 1)
        end

    else assembler 
                (IntMap.add ticks 
                                {n=(getNeighbors (nrow+1) ncol aspan neighbor_fns); v=(Random.bool ()); row=(nrow+1); col=ncol}
                                box)
                (nrow+1) ncol (ticks+1)
  in

   assembler 
;;




(* The core metric in Life is the count of the live 'neighbor' cells around a
  given cell. 
 
  In the cellrec there is a list of neighbor cells, pre-filled as a part of
  program initialization.
 
  Given this list of sequence numbers, scan through, looking up the cellrec
  associated with each of sequence number.
 
  if that cellrec has a LIVE status, accumulate the sequence number into a list.
 
  Finally, return the COUNT of accumulated list of all sequence numbers of cells that
  were LIVE.  *)

let getLiveCount  (ws: cellrec IntMap.t)  (seq_list: int list) =

  let rec seqscan xs result =
    match xs with
      |  [] -> result
      |  hd::tl ->  if ((IntMap.find hd ws).v : bool) && true then seqscan tl (hd :: result) else seqscan tl result;
  in
    List.length (seqscan seq_list [] ) ;;




(* this looks at an existing cellrec and determines its next state 
 
  Note: those last couple if true-case match criteria could be consolidated but are kept discrete at
  the moment in order to help interpret profiling stats *)

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
                  mspan=200; 
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
                    ("bw", {bcolor = (rgb 40 40 40); fcolor = (rgb 220 213 194)});
                    ("moo", {bcolor = (rgb 4 4 4); fcolor = (rgb 0 170 0)})  ] ;;
 


(* It is convenient to store the theme data in a map, using the theme name as
  the key.  This way you can quickly reference individual cell records  

  So, first fill a lookup map with theme info from the referenc list *)

let rec mapfiller xs box =
  match xs with
    | [] -> box
    | hd::tl -> mapfiller  tl (ThemeRepo.add (fst hd) (snd hd) box) ;;



(* and then here is the filled theme map value *)

let themes = mapfiller  current_themes  ThemeRepo.empty;;



(* dev *)
let theme_printer key xrec = 
  print_string("\nkey: " ^  key) ;;

(* ThemeRepo.iter theme_printer themes;; *)



(* returns a cell rendering function, with the current game environment baked-in  *)

let getFillPlotter env =

  (fun key xrec -> begin
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
  a cellrec containing the new cell state. *)

let getReaper xs = 
  (fun c -> {n=c.n; v=(getNewState xs c.n c.v); row=c.row; col=c.col;}) ;;



(* this runner fn is where the bulk of the  work is done *)

let runner reps xs =

  let env = !run_env in
  let renderer = (getFillPlotter env) in 
  
  (* note that the IntMap iterator calls for a custom reaper (using current workset) function... *)
  let rec looper ticks ws =
    IntMap.iter renderer ws;  
    synchronize ();

    if ( key_pressed () = false) && (ticks < reps) then looper (ticks + 1) 
                                                               (IntMap.map (getReaper ws) ws) else ticks in 

  looper 0 xs ;;



(* make ready a graphic surface, with padding. *)

let init_graphics env =
  let gridspan = ( (2 * env.padding) + (env.zoom * env.mspan)) in

    open_graph (" " ^ (string_of_int gridspan) ^ "x" ^ (string_of_int gridspan));
    auto_synchronize false;
    set_window_title "life.ml";
    clear_the_dancefloor env ;
    at_exit (fun () -> print_string "\n-------> Life.eol \n"; close_graph ());;



(* all of these environment setter functions are called by the Arg handler. 
   There's an abstraction hiding around here somewhere... *)

let set_span n =
  print_string("\nspan: " ^ (string_of_int n));
  run_env := {zoom = !run_env.zoom; mspan = n; padding = !run_env.padding; bkcolor = !run_env.bkcolor; fgcolor = !run_env.fgcolor} ;;

let set_zoom n =
  print_string(", zoom: " ^ (string_of_int n));
  run_env := {zoom = n; mspan = !run_env.mspan; padding = !run_env.padding; bkcolor = !run_env.bkcolor; fgcolor = !run_env.fgcolor} ;;


let set_theme x  =
  try
    let t_rec = ThemeRepo.find x themes in
      run_env :=  {zoom = !run_env.zoom; mspan = !run_env.mspan; padding = !run_env.padding; bkcolor = t_rec.bcolor; fgcolor = t_rec.fcolor}  
  with Not_found -> print_endline("\nSorry, there is no theme named " ^ x ^ ", using the default theme...")
;; 


let get_mode_fn z m =
  (fun () ->  run_env := {zoom = z; mspan = m; padding = !run_env.padding; bkcolor = !run_env.bkcolor; fgcolor = !run_env.fgcolor}) 
;;



(* command line specification, using the standard lib Arg module *)

let speclist = [( "-regular", Arg.Unit (get_mode_fn 3 200), "(preset, with span: 200 and zoom: 3)\n");
                ( "-rapido", Arg.Unit  (get_mode_fn 6 120), "(preset, with span: 120 and zoom: 6)\n");
                ( "-medium", Arg.Unit (get_mode_fn 2 300), "(preset, with span: 300 and zoom: 2)\n");
                ( "-jumbo", Arg.Unit (get_mode_fn 2 600), "(preset, with span: 600 and zoom: 2)\n");
                ( "-mega", Arg.Unit (get_mode_fn 1 1000), "(preset, with span: 1000 and zoom: 1)\n");
                ( "-t", Arg.String (set_theme), "use a preset color theme.  Options: [ cosmic, moo, oldskool, bw, amberish]\n");
                ( "-s", Arg.Int (set_span), "set custom span size. \n");
                ( "-z", Arg.Int (set_zoom), "set custom zoom value  \n"); ] ;;




(* Probably crude but help text is provided via this function *)

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



(* main app entry point.
   complete with a somewhat smelly way of handling help text. *)

let () =

  Arg.parse speclist (fun a -> ()) (get_help_text) ;

  init_graphics !run_env;

  let grid_assembler = get_grid_assembler !run_env.mspan xfuns in
  let work_set = grid_assembler IntMap.empty 0 1 1 in
  let start_ts = Sys.time() in
  let completed_reps = (runner max_int work_set) in
  let end_ts = Sys.time() in
    print_string("\n\n generations: " ^ string_of_int completed_reps);
    print_string( Printf.sprintf "\n total runtime: %.2f" (end_ts -. start_ts));
    print_string( Printf.sprintf "\n GPS:  %.2f" ( (float_of_int completed_reps) /.  (end_ts -.  start_ts)))
;;


 read_line  ();;


