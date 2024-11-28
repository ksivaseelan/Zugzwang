export function get_route() {
  return window.location.pathname;
}

export function copy_game_id(text: string): void {
  navigator.clipboard
    .writeText(text)
    .then(() => {
      console.log("Game ID copied to clipboard");
    })
    .catch((err) => {
      console.error("Failed to copy: ", err);
    });
}
