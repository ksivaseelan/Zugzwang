// import gleam/string
// import gleam/dynamic.{type Dynamic}
// import gleam/option.{type Option, None, Some}
// import gleam/dict.{type Dict}
// import gleam/int
// import gleam/result

import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/io
import gleam/list
import gleam/result

@external(erlang, "binbo", "start")
pub fn start() -> Result(start, err)

@external(erlang, "binbo", "new_server")
pub fn new_server() -> Result(pid, err)

@external(erlang, "binbo", "new_game")
pub fn new_game(pid: Pid) -> Result(new_game, err)

@external(erlang, "binbo", "print_board")
pub fn print_board(pid: Pid) -> Nil

@external(erlang, "binbo", "move")
pub fn move(pid: Pid, move: String) -> Result(move, err)

// possible promotion piece are q, r, b, n
// use atom.create_from_string to create the atom
@external(erlang, "binbo", "move")
pub fn move_promo(
  pid: Pid,
  move: String,
  promotion_piece: Atom,
) -> Result(move, err)

@external(erlang, "binbo", "index_move")
pub fn index_move(
  pid: Pid,
  from_square: Int,
  to_square: Int,
) -> Result(move, err)

// possible promotion piece are q, r, b, n
// use atom.create_from_string to create the atom
@external(erlang, "binbo", "index_move")
pub fn index_move_promo(
  pid: Pid,
  from_square: Int,
  to_square: Int,
  promotion_piece: Atom,
) -> Result(move, err)

@external(erlang, "binbo", "game_status")
pub fn game_status(pid: Pid) -> Result(game_status, err)

@external(erlang, "binbo", "all_legal_moves")
pub fn all_legal_moves(pid: Pid) -> Result(List(#(Int, Int)), err)

//  pass the atom bin to get the list of legal moves as strings
// let bin = atom.create_from_string("bin")
@external(erlang, "binbo", "all_legal_moves")
pub fn all_legal_moves_as_strings(
  pid: Pid,
  move_type: Atom,
) -> Result(List(#(Int, Int)), err)

pub fn play() {
  use _ <- result.try(start())
  use pid <- result.try(new_server())
  use _ <- result.try(new_game(pid))
  Ok(pid)
}

// list of moves for a square
pub fn click_square(pid: Pid, square: Int) -> List(#(Int, Int)) {
  case all_legal_moves(pid) {
    Ok(moves) -> moves |> list.filter(fn(move) { move.0 == square })
    Error(err) -> err
  }
}

pub fn make_move(pid: Pid, from_square: Int, to_square: Int) {
  let assert Ok(move) = index_move(pid, from_square, to_square)
  move
}

pub fn main() {
  todo
  //  let assert Ok(pid) = play()
  //  let assert Ok(_) = move(pid, "e2e4")
  //  print_board(pid)
}
