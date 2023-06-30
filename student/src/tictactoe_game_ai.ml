open! Core
open Tic_tac_toe_2023_common
open Protocol

(* Exercise 1.2.

   Implement a game AI that just picks a random available position. Feel free
   to raise if there is not an available position.

   After you are done, update [compute_next_move] to use your
   [random_move_strategy]. *)
let random_move_strategy
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let possible_positions =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  match possible_positions with
  | [] -> failwith "No available moves!"
  | _ -> List.random_element_exn possible_positions
;;

(* Exercise 3.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let _pick_winning_move_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  match List.is_empty winning with
  | true -> random_move_strategy ~game_kind ~pieces
  | false -> List.random_element_exn winning
;;

(* disables unused warning. Feel free to delete once it's used. let _ =
   pick_winning_move_if_possible_strategy *)

(* Exercise 4.2.

   Implement a game AI that picks a random position, unless there is an
   available winning move.

   After you are done, update [compute_next_move] to use your
   [pick_winning_move_if_possible_strategy]. *)
let pick_winning_move_or_block_if_possible_strategy
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let winning =
    Tic_tac_toe_exercises_lib.winning_moves ~me ~game_kind ~pieces
  in
  let losing =
    Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
  in
  match List.is_empty winning with
  | false -> List.random_element_exn winning
  | true ->
    if not (List.is_empty losing)
    then List.random_element_exn losing
    else random_move_strategy ~game_kind ~pieces
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let current_state =
    Tic_tac_toe_exercises_lib.evaluate ~game_kind ~pieces
  in
  match current_state with
  | Tic_tac_toe_exercises_lib.Evaluation.Game_over { winner = Some x } ->
    if Piece.equal x me then Float.infinity else Float.neg_infinity
  | _ -> 0.0
;;

let _ = score

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)

let rec minimax_helper
  ~(me : Piece.t)
  ~(depth : int)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  let current_score = score ~me ~game_kind ~pieces in
  let avail_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  if (not (Float.equal current_score 0.0))
     || depth = 0
     || List.is_empty avail_moves
  then current_score *. -1.
  else (
    let bestscore, _bestmove =
      List.fold
        avail_moves
        ~init:(0., None)
        ~f:(fun (some_score, some_pos) current_pos ->
        let new_score =
          minimax_helper
            ~me:(Piece.flip me)
            ~depth:(depth - 1)
            ~game_kind
            ~pieces:(Map.set pieces ~key:current_pos ~data:me)
        in
        if Float.( >= ) new_score some_score
        then new_score, Some current_pos
        else some_score, some_pos)
    in
    bestscore *. -1.0)
;;

let minimax
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : Position.t
  =
  let _current_score = score ~game_kind ~pieces in
  let avail_moves =
    Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces
  in
  let _best_score, best_move =
    List.fold
      avail_moves
      ~init:(0.0, None)
      ~f:(fun (highestscore, highestmove) pos ->
      let inprogresscalc =
        minimax_helper
          ~me:(Piece.flip me)
          ~game_kind
          ~depth:6
          ~pieces:(Map.set pieces ~key:pos ~data:me)
      in
      if Float.( >= ) inprogresscalc highestscore
      then inprogresscalc, Some pos
      else highestscore, highestmove)
  in
  match best_move with
  | None -> { Position.row = 0; column = 0 }
  | Some pos -> pos
;;

let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  (* pick_winning_move_or_block_if_possible_strategy ~me ~game_kind
     ~pieces *)
  minimax ~me ~game_kind ~pieces
;;

(* let max_player = match game_status with | Turn_of Some x -> if Piece.equal
   x me then true else false | _ -> false in let which_piece = if max_player
   then me else Piece.flip me in if current_score <> 0.0 || List.is_empty
   avail_moves then current_score else let moves_with_scores = List.map
   avail_moves ~f:(fun position -> (let score = (minimax_helper ~which_piece
   ~depth:6 ~game_kind ~pieces: Map.set pieces ~key:position ~data:
   which_piece) in (position, score)) in let best_move = if max_player then
   List.max_elt moves_with_scores ~compare:() else List.min_elt
   moves_with_scores ~compare:() in best_move

   ;; *)

(* let optimal ~game_kind ~pieces = let avail_moves =
   (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces) in let
   new_position_value = List.map avail_moves ~f:(fun positon -> (minimax
   ~depth:0.0 ~is_maximizing_player:false ~game_kind ~pieces)) *)
