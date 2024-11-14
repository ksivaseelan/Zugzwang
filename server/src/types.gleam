import gleam/erlang/process.{type Pid, type Subject}
import gleam/option.{type Option}

pub type Color {
  Black
  White
}

pub type Player {
  Player(color: Color, id: String)
}

pub type Status {
  Initializing
  WaitingForPlayer
  InProgress
  Completed
}

pub type Turn {
  Turn(color: Color)
}

pub type ChessMessage {
  CreateGame(
    reply_with: Subject(Result(GameState, GameError)),
    chosen_color: Color,
    player_id: String,
  )
  JoinGame(
    reply_with: Subject(Result(GameState, GameError)),
    player_id: String,
    game_id: String,
  )
  // MakeMove(player_id: String, move_details: String)
  // GetGameState(
  //   reply_with: Subject(Result(GameState, GameError)),
  //   player_id: String,
  // )
  // PlayerDisconnect(player_id: String)
  // EndGame(game_id: String, reason: String)
}

pub type GameState {
  GameState(
    player1: Option(Player),
    player2: Option(Player),
    id: String,
    pid: Pid,
    status: Status,
    turn: Turn,
  )
}

pub type GameError {
  GameFull
  GameNotFound
  InvalidColor
  GameCreationTimeout
  GameInitError
}
