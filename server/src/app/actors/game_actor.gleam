// import game
// import gleam/erlang/process.{type Pid}
// import gleam/io
// import gleam/option.{None, Some}
// import gleam/otp/actor.{type Next}
// import logging
// import types.{
//   type ChessMessage, type GameState, Black, CreateGame, GameNotFound, GameState,
//   InProgress, JoinGame, Player, Turn, WaitingForPlayer, White,
// }
// import youid/uuid

// pub fn start(game_id: String) {
//   logging.log(logging.Info, "Starting Game Manager Actor")
//   let initial_state =
//     GameState(
//       player1: None,
//       player2: None,
//       id: game_id,
//       pid: process.self(),
//       status: WaitingForPlayer,
//       turn: Turn(White),
//     )
//   let assert Ok(actor) = actor.start(initial_state, handle_message)
//   actor
// }

// pub fn handle_message(
//   message: ChessMessage,
//   state: GameState,
// ) -> Next(ChessMessage, GameState) {
//   case message {
//     CreateGame(reply_with, chosen_color, player_id) ->
//       case state {
//         _ -> {
//           //create game with first player
//           let new_player = Player(chosen_color, player_id)
//           let assert Ok(pid) = game.play()
//           let id = uuid.v4_string()
//           let new_state =
//             types.GameState(
//               player1: Some(new_player),
//               player2: None,
//               id: id,
//               pid: pid,
//               status: WaitingForPlayer,
//               turn: Turn(White),
//             )

//           process.send(reply_with, Ok(new_state))
//           actor.continue(new_state)
//         }
//       }
//     JoinGame(client, player_id, new_game_id) -> {
//       case state, new_game_id {
//         types.GameState(
//           status: WaitingForPlayer,
//           player1: Some(existing_player),
//           id: game_id,
//           pid: pid,
//           turn: Turn(White),
//           ..,
//         ),
//           new_game_id
//         -> {
//           case new_game_id == game_id {
//             True -> {
//               let new_color = case existing_player.color {
//                 White -> Black
//                 Black -> White
//               }

//               let new_player = Player(new_color, player_id)
//               let new_state =
//                 GameState(
//                   status: InProgress,
//                   id: game_id,
//                   pid: pid,
//                   player1: Some(existing_player),
//                   player2: Some(new_player),
//                   turn: Turn(White),
//                 )
//               process.send(client, Ok(new_state))
//               actor.continue(new_state)
//             }
//             False -> {
//               //game doesn't exist
//               io.println("Error: Game doesn't exist")
//               process.send(client, Error(GameNotFound))
//               actor.continue(state)
//             }
//           }
//         }
//         _, _ -> {
//           //error unjoinable game
//           io.println("Error: Game is unjoinable")
//           actor.continue(state)
//         }
//       }
//     }
//     // GetGameState(client, player_id), state -> {
//     //   case state, player_id {
//     //     NotStarted, _player_id -> {
//     //       //error Game does not exist
//     //       io.println("Error: Game does not exist")
//     //       actor.continue(state)
//     //     }
//     //     GameState(player1: player1, player2: player2, ..), player_id -> {
//     //       //verify requested player is in the game
//     //       case player1, player2 {
//     //         Some(player1), Some(player2) -> {
//     //           case player1.id == player_id || player2.id == player_id {
//     //             True -> {
//     //               io.debug("Returning game state for player: " <> player_id)
//     //               io.debug(state)
//     //               actor.continue(state)
//     //             }
//     //             False -> {
//     //               //error player not in game
//     //               io.println("Error: Player not found in game")
//     //               actor.continue(state)
//     //             }
//     //           }
//     //         }
//     //         Some(player1), None -> {
//     //           case player1.id == player_id {
//     //             True -> {
//     //               io.debug("Returning game state for player: " <> player_id)
//     //               io.debug(state)
//     //               process.send(client, Ok(state))
//     //               actor.continue(state)
//     //             }
//     //             False -> {
//     //               //error player not in game
//     //               io.println("Error: Player not found in game")
//     //               actor.continue(state)
//     //             }
//     //           }
//     //         }
//     //         None, Some(player2) -> {
//     //           case player2.id == player_id {
//     //             True -> {
//     //               io.debug("Returning game state for player: " <> player_id)
//     //               io.debug(state)
//     //               process.send(client, Ok(state))
//     //               actor.continue(state)
//     //             }
//     //             False -> {
//     //               //error player not in game
//     //               io.println("Error: Player not found in game")
//     //               actor.continue(state)
//     //             }
//     //           }
//     //         }
//     //         _, _ -> {
//     //           //error player not in game
//     //           io.println("Error: Player not found in game")
//     //           actor.continue(state)
//     //         }
//     //       }
//     //     }
//     //   }
//     // }
//   }
// }
