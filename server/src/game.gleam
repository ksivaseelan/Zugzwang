import gleam/erlang/process.{type Pid, type Subject}
import gleam/io
import gleam/option.{type Option, None, Some}
import gleam/otp/actor
import gleam/result
import gleam/string
import gleam_binbo
import mist.{type WebsocketConnection}

//GameState can be Waiting, Playing or Finished
//binbo_pid is the pid of the binbo process for calling binbo functions
//w_player is the player who is White
//b_player is the player who is Black

pub type GameState {
  Waiting(
    game_id: String,
    binbo_pid: Pid,
    w_player: Option(WebsocketConnection),
    b_player: Option(WebsocketConnection),
  )
  Playing(
    game_id: String,
    binbo_pid: Pid,
    w_player: WebsocketConnection,
    b_player: WebsocketConnection,
  )
  Finished(
    game_id: String,
    binbo_pid: Pid,
    w_player: WebsocketConnection,
    b_player: WebsocketConnection,
    result: String,
  )
}

//GameMessage can be Move, Join
pub type GameMessage(msg) {
  Shutdown
  Move(move: msg)
  Join(reply_with: Subject(Result(msg, Nil)))
}

pub type Color {
  White
  Black
}

//init function takes game_id, conn and color and starts a binbo process
pub fn init(
  game_id: String,
  conn: WebsocketConnection,
  color: Color,
) -> GameState {
  let assert Ok(pid) = gleam_binbo.play()
  case color {
    White -> Waiting(game_id, pid, Some(conn), None)
    Black -> Waiting(game_id, pid, None, Some(conn))
  }
}

//handle_message function takes current_state and message and returns a new state
pub fn handle_message(message: GameMessage(msg), current_state: GameState) {
  case current_state, message {
    //Shutdown message is sent
    _, Shutdown -> actor.Stop(process.Normal)
    //Join message is sent by the black player
    //Game state is changing to Playing
    Waiting(game_id, binbo_pid, Some(w_player), None), Join(conn) -> {
      io.debug(" Game actor: Move recieved")
      let new_state = Playing(game_id, binbo_pid, w_player, conn)
      let assert Ok(_) =
        mist.send_text_frame(w_player, "Game started: You are White")
      let assert Ok(_) =
        mist.send_text_frame(conn, "Game started: You are Black")
      actor.continue(new_state)
    }
    //Join message is sent by the white player 
    //Game state is changing to Playing
    Waiting(game_id, binbo_pid, None, Some(b_player)), Join(conn) -> {
      let new_state = Playing(game_id, binbo_pid, conn, b_player)
      let assert Ok(_) =
        mist.send_text_frame(conn, "Game started: You are White")
      let assert Ok(_) =
        mist.send_text_frame(b_player, "Game started: You are Black")
      actor.continue(new_state)
    }
    Waiting(..), Move(_, conn) -> {
      let assert Ok(_) =
        mist.send_text_frame(conn, "Game Error: Waiting for players to join")
      actor.continue(current_state)
    }
    Playing(game_id, binbo_pid, w_player, b_player), Move(move, conn) -> {
      case gleam_binbo.move(binbo_pid, move) {
        Ok(new_board) -> {
          gleam_binbo.print_board(new_board)
          let assert Ok(_) =
            mist.send_text_frame(w_player, "Move Success" <> move)
          let assert Ok(_) =
            mist.send_text_frame(b_player, "Move Success" <> move)
          actor.continue(Playing(game_id, new_board, w_player, b_player))
        }
        Error(_) -> {
          let assert Ok(_) =
            mist.send_text_frame(conn, "Move Error: Not a legal move")
          actor.continue(current_state)
        }
      }
    }
    _, _ -> actor.continue(current_state)
  }
}
