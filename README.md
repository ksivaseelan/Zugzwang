# Zugzwang  

Welcome to **Zugzwang**! This is my passion project—a fun, web-based chess game where you can challenge a friend by sharing a simple link. Built with a modern, snazzy tech stack, Zugzwang is all about making chess easy to play and enjoy online. Let’s get you rolling! ♟️✨  

## Table of Contents  
- [Zugzwang](#zugzwang)
  - [Table of Contents](#table-of-contents)
  - [Project Overview](#project-overview)
  - [Tech Stack](#tech-stack)
    - [Server](#server)
    - [Client](#client)
  - [Monorepo Structure](#monorepo-structure)
  - [Getting Started](#getting-started)
    - [Running the Server](#running-the-server)
    - [Running the Client](#running-the-client)
  - [License](#license)

## Project Overview  

**Zugzwang** lets two players battle it out in chess by simply sharing a game link. The backend manages the game logic, while the frontend brings the board to life with smooth animations and a clean UI. Whether you're a chess master or just playing for fun, Zugzwang is here for you!  

## Tech Stack  

### Server  
- **Language:** [Gleam](https://gleam.run)  
- **HTTP Server:** [Mist](https://github.com/rawhat/mist)  

### Client  
- **Language:** [Gleam](https://gleam.run)  
- **Frontend Framework:** [Lustre](https://github.com/lustre-labs/lustre)  
- **Graphics Library:** [PixiJS](https://pixijs.com)  
- **Build Tool:** [Vite](https://vite.dev)  

## Monorepo Structure  

Here’s the simple and tidy layout of the project:  
```
server/ - Backend server code (Gleam + Mist)
client/ - Frontend code (Gleam + Lustre + PixiJS + Vite)
```
## Getting Started  

### Running the Server  

The server is the brain of the operation, handling all the chess logic like game creation and keeping track of moves.  

```bash  
gleam run
```
### Running the Client
The client is where the magic happens: it renders the chessboard, animations, and handles player interactions.
```bash
bun dev  
```
Now open your browser, share a game link with a friend, and let the chess duels begin! 🥳

## License
This project is open-source under the MIT License, so feel free to use it, tweak it, or share it. Let’s make chess even better together!

Enjoy playing Zugzwang as much as I loved building it! And remember: the only thing better than winning is having a good game. Happy chessing! ♟️😊