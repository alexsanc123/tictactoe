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
let pick_winning_move_if_possible_strategy
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
  let losing =
    Tic_tac_toe_exercises_lib.losing_moves ~me ~game_kind ~pieces
  in
  match List.is_empty losing with
  | false -> List.random_element_exn losing
  | true -> pick_winning_move_if_possible_strategy ~me ~game_kind ~pieces
;;

(* disables unused warning. Feel free to delete once it's used. *)
let _ = pick_winning_move_or_block_if_possible_strategy

let score
  ~(me : Piece.t)
  ~(game_kind : Game_kind.t)
  ~(pieces : Piece.t Position.Map.t)
  : float
  =
  ignore me;
  ignore game_kind;
  ignore pieces;
  0.0
;;

let _ = score

(* [compute_next_move] is your Game AI's function.

   [game_ai.exe] will connect, communicate, and play with the game server,
   and will use [compute_next_move] to pick which pieces to put on your
   behalf.

   [compute_next_move] is only called whenever it is your turn, the game
   isn't yet over, so feel free to raise in cases where there are no
   available spots to pick. *)
let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  pick_winning_move_or_block_if_possible_strategy ~me ~game_kind ~pieces
;;
