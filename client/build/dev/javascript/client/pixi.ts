import { Application, Assets, Sprite, FederatedPointerEvent } from "pixi.js";

// Interface extending the Sprite interface to store the starting X and Y positions
interface DataSprite extends Sprite {
  data: { startX: number; startY: number };
}

export async function main() {
  const app = new Application();
  await app.init({ width: 784, height: 784 });
  document.body.appendChild(app.canvas);

  // --- WebSocket Connection ---
  const socket = new WebSocket("ws://localhost:6969/ws");

  socket.onopen = async () => {
    console.log("WebSocket connection opened");
    // Send an initial message or perform other actions upon connection
  };

  socket.onmessage = async (event) => {
    console.log("Message from server:", event.data);
    // Handle incoming messages from the server
    // Example: Update game state based on server data
  };

  socket.onclose = async () => {
    console.log("WebSocket connection closed");
    // Handle connection closure (e.g., reconnect logic)
  };

  socket.onerror = async (error) => {
    console.error("WebSocket error:", error);
    // Handle WebSocket errors
  };

  await Assets.load("board.png");
  const board = Sprite.from("board.png");
  board.anchor.set(0.5);
  board.x = app.screen.width / 2;
  board.y = app.screen.height / 2;
  app.stage.addChild(board);

  const squareSize = board.width / 8;

  const initialPositions = [
    { row: 0, col: 0, piece: "pieces/black-rook.png" },
    { row: 0, col: 1, piece: "pieces/black-knight.png" },
    { row: 0, col: 2, piece: "pieces/black-bishop.png" },
    { row: 0, col: 3, piece: "pieces/black-queen.png" },
    { row: 0, col: 4, piece: "pieces/black-king.png" },
    { row: 0, col: 5, piece: "pieces/black-bishop.png" },
    { row: 0, col: 6, piece: "pieces/black-knight.png" },
    { row: 0, col: 7, piece: "pieces/black-rook.png" },
    { row: 1, col: 0, piece: "pieces/black-pawn.png" },
    { row: 1, col: 1, piece: "pieces/black-pawn.png" },
    { row: 1, col: 2, piece: "pieces/black-pawn.png" },
    { row: 1, col: 3, piece: "pieces/black-pawn.png" },
    { row: 1, col: 4, piece: "pieces/black-pawn.png" },
    { row: 1, col: 5, piece: "pieces/black-pawn.png" },
    { row: 1, col: 6, piece: "pieces/black-pawn.png" },
    { row: 1, col: 7, piece: "pieces/black-pawn.png" },
    { row: 6, col: 0, piece: "pieces/white-pawn.png" },
    { row: 6, col: 1, piece: "pieces/white-pawn.png" },
    { row: 6, col: 2, piece: "pieces/white-pawn.png" },
    { row: 6, col: 3, piece: "pieces/white-pawn.png" },
    { row: 6, col: 4, piece: "pieces/white-pawn.png" },
    { row: 6, col: 5, piece: "pieces/white-pawn.png" },
    { row: 6, col: 6, piece: "pieces/white-pawn.png" },
    { row: 6, col: 7, piece: "pieces/white-pawn.png" },
    { row: 7, col: 0, piece: "pieces/white-rook.png" },
    { row: 7, col: 1, piece: "pieces/white-knight.png" },
    { row: 7, col: 2, piece: "pieces/white-bishop.png" },
    { row: 7, col: 3, piece: "pieces/white-queen.png" },
    { row: 7, col: 4, piece: "pieces/white-king.png" },
    { row: 7, col: 5, piece: "pieces/white-bishop.png" },
    { row: 7, col: 6, piece: "pieces/white-knight.png" },
    { row: 7, col: 7, piece: "pieces/white-rook.png" },
  ];

  for (const pos of initialPositions) {
    await Assets.load(pos.piece);
    const sprite = await Sprite.from(pos.piece);
    sprite.anchor.set(0.5);
    sprite.x = (pos.col + 0.5) * squareSize;
    sprite.y = (pos.row + 0.5) * squareSize;
    sprite.scale.set(0.8);
    sprite.eventMode = "static";
    sprite.cursor = "pointer";
    sprite.on("pointerdown", onDragStart, sprite);

    app.stage.addChild(sprite);
  }

  let dragTarget: DataSprite | null = null;

  app.stage.eventMode = "static";
  app.stage.hitArea = app.screen;
  app.stage.on("pointerup", onDragEnd);
  app.stage.on("pointerupoutside", onDragEnd);

  let isDragging = false;

  await sendCreate(socket);

  function onDragMove(event: FederatedPointerEvent) {
    if (dragTarget && isDragging) {
      dragTarget.parent.toLocal(event.global, undefined, dragTarget.position);
    }
  }

  function onDragStart(this: DataSprite) {
    isDragging = true;
    // Store a reference to the data
    // * The reason for this is because of multitouch *
    // * We want to track the movement of this particular touch *
    this.alpha = 0.5;
    dragTarget = this as DataSprite;
    dragTarget.data = {
      startX: Math.floor(this.x / squareSize),
      startY: Math.floor(this.y / squareSize),
    };
    console.log(
      `From: X: ${dragTarget.data.startX} Y: ${dragTarget.data.startY}`
    );
    app.stage.on("pointermove", onDragMove);
  }

  function onDragEnd() {
    isDragging = false;
    if (dragTarget) {
      app.stage.off("pointermove", onDragMove);
      // reset tranparency of dragging piece
      dragTarget.alpha = 1;

      // Calculate the grid position the piece should snap to
      const endX = Math.floor(dragTarget.x / squareSize);
      const endY = Math.floor(dragTarget.y / squareSize);

      const startCol = String.fromCharCode(97 + dragTarget.data.startX); // Convert column index to letter
      const startRow = 8 - dragTarget.data.startY; // Convert row index (0-7) to chess row (8-1)
      const endCol = String.fromCharCode(97 + endX);
      const endRow = 8 - endY;

      const moveNotation = `${startCol}${startRow}${endCol}${endRow}`;
      console.log(moveNotation);

      const moveMessage = "Move: " + moveNotation;
      sendMoves(moveMessage, socket);

      // Snap the piece to the center of the grid position
      dragTarget.x = (endX + 0.5) * squareSize;
      dragTarget.y = (endY + 0.5) * squareSize;

      dragTarget = null;
    }
  }
}

//   let elapsed = 0.0;
//   app.ticker.add((ticker) => {
//     elapsed += ticker.deltaTime / 60.0;
//     if (elapsed >= sprite.texture.width) {
//       elapsed = 0.0;
//     }
//     sprite.x = elapsed;
//   });

async function sendMoves(moveMessage: string, socket: WebSocket) {
  socket.send(moveMessage);
}

async function sendCreate(socket: WebSocket) {
  socket.send("Create");
}
