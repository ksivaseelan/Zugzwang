// import app/actors/game_actor
// import game
// import gleam/dict.{type Dict}
// import gleam/erlang/process.{type Pid, type Subject}
// import gleam/io
// import gleam/option.{None, Some}
// import gleam/otp/actor.{type Next}
// import logging
// import types.{
//   type ChessMessage, type GameError, type GameState, Black, CreateGame,
//   GameInitError, GameNotFound, GameState, InProgress, JoinGame, Player, Turn,
//   WaitingForPlayer, White,
// }
// import youid/uuid

// pub type GameManagerState {
//   GameManagerState(active_games: Dict(String, Pid))
// }

// pub fn start() {
//   logging.log(logging.Info, "Starting Game Manager Actor")
//   let state = GameManagerState(active_games: dict.new())
//   let assert Ok(actor) = actor.start(state, handle_message)
//   actor
// }

// fn handle_message(
//   message: ChessMessage,
//   state: GameManagerState,
// ) -> Next(ChessMessage, GameManagerState) {
//   case message {
//     CreateGame(reply_with, chosen_color, player_id) -> {
//       let game_id = uuid.v4_string()
//       let game_actor = game_actor.start(game_id)
//       let new_state =
//         GameManagerState(active_games: dict.insert(
//           state.active_games,
//           game_id,
//           game_actor,
//         ))
//       process.send(reply_with, game_actor)
//       actor.continue(new_state)
//     }
//     JoinGame(reply_with, player_id, game_id) -> {
//       case dict.get(state.active_games, game_id) {
//         Some(game_actor) ->
//           game_actor.send(JoinGame(reply_with, player_id, game_id))
//         None -> {
//           reply_with.send(Error(GameNotFound))
//           actor.continue(state)
//         }
//       }
//     }
//     _ -> actor.continue(state)
//   }
// }
