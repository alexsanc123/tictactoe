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

let compute_next_move ~(me : Piece.t) ~(game_state : Game_state.t)
  : Position.t
  =
  let pieces = game_state.pieces in
  let game_kind = game_state.game_kind in
  pick_winning_move_or_block_if_possible_strategy ~me ~game_kind ~pieces


;;


let are_moves_left ~game_kind ~pieces =
  List.is_empty (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces) ;;



let rec minimax 
~(depth: int)
~(me: Piece.t)
~(game_state : Game_state.t) 
= 
match Tic_tac_toe_exercises_lib.evaluate ~game_kind:game_state.game_kind ~pieces: game_state.pieces with
| Tic_tac_toe_exercises_lib.Evaluation.Game_over {winner = Some x} -> (score ~me:x ~game_kind:game_state.game_kind ~pieces:game_state.pieces)
| _ -> (if ~depth = 0 then (score ~me:x ~game_kind:game_state.game_kind ~pieces:game_state.pieces) 
                      else (if 
                        (are_moves_left ~game_kind:game_state.game_kind ~pieces:game_state.pieces) 
                        then (let avail_moves = Tic_tac_toe_exercises_lib.available_moves ~game_kind:game_state.game_kind ~pieces:game_state.pieces in 
                        let current_score = score ~me:x ~game_kind:game_state.game_kind ~pieces:game_state.pieces in 
                        List.map avail_moves ~f:(fun position -> (let new_board = Map.set ~pieces ~key: position ~data:me 
                      in score ~me:x ~game_kind:game_state.game_kind ~pieces:new_board)
                      )) 
                      else score ~me:x ~game_kind:game_state.game_kind ~pieces:game_state.pieces))




let current_score = score ~game_kind ~pieces in
match current_score <> 0.0 with 
| true -> current_score
| false -> 
  match are_moves_left ~game_kind ~pieces with
  | false -> 0.0
  | true -> if is_maximizing_player then 



;;


let optimal ~game_kind ~pieces =
  let avail_moves = (Tic_tac_toe_exercises_lib.available_moves ~game_kind ~pieces) in
  let new_position_value = List.map avail_moves ~f:(fun positon -> (minimax ~depth:0.0 ~is_maximizing_player:false ~game_kind ~pieces))

