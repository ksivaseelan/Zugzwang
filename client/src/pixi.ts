import {
  Application,
  Assets,
  Sprite,
  Container,
  Point,
  FederatedPointerEvent,
} from "pixi.js";

export async function main() {
  
  const app = new Application();
  await app.init({ width: 784, height: 784 });
  document.body.appendChild(app.canvas);

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

  let dragTarget: Sprite | null = null;

  app.stage.eventMode = "static";
  app.stage.hitArea = app.screen;
  app.stage.on("pointerup", onDragEnd);
  app.stage.on("pointerupoutside", onDragEnd);

  let isDragging = false;

  function onDragMove(event: FederatedPointerEvent) {
    if (dragTarget && isDragging) {
      dragTarget.parent.toLocal(event.global, undefined, dragTarget.position);
    }
  }

  function onDragStart() {
    isDragging = true;
    // Store a reference to the data
    // * The reason for this is because of multitouch *
    // * We want to track the movement of this particular touch *
    this.alpha = 0.5;
    dragTarget = this;
    app.stage.on("pointermove", onDragMove);
  }

  function onDragEnd() {
    isDragging = false;
    if (dragTarget) {
      app.stage.off("pointermove", onDragMove);
      dragTarget.alpha = 1;

      // Calculate the grid position the piece should snap to
      const gridX = Math.floor(dragTarget.x / squareSize);
      const gridY = Math.floor(dragTarget.y / squareSize);

      // Snap the piece to the center of the grid position
      dragTarget.x = (gridX + 0.5) * squareSize;
      dragTarget.y = (gridY + 0.5) * squareSize;
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
