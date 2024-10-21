# Chess Monorepo

Welcome to my **Chess Monorepo**! This project is an online chess game where you can challenge a friend by sharing a game link. I built this with a modern tech stack that’s both fun to work with and effective for delivering a smooth chess experience. Let’s dive in! 🏆

## Table of Contents
- [Chess Monorepo](#chess-monorepo)
  - [Table of Contents](#table-of-contents)
  - [Project Overview](#project-overview)
  - [Tech Stack](#tech-stack)
    - [Server](#server)
    - [Client](#client)
  - [Monorepo Structure](#monorepo-structure)
  - [Running the Application](#running-the-application)
    - [Server](#server-1)
    - [Client](#client-1)
  - [Contributing](#contributing)
  - [License](#license)

## Project Overview

This is a web-based chess game that allows two players to compete by sharing a game link. The backend is built using **Gleam** and **Wisp, while the frontend is powered by **Gleam**, **Lustre**, and **Vite**. It handles all the necessary logic and interactions, creating a dynamic and enjoyable chess experience.

## Tech Stack

### Server
- **Language:** [Gleam](https://gleam.run)
- **Framework:** [Wisp](https://github.com/gleam-wisp/wisp)
- **HTTP Webserver:** [Mist](https://github.com/rawhat/mist)

### Client
- **Language:** [Gleam](https://gleam.run)
- **Frontend Framework:** [Lustre](https://github.com/lustre-labs/lustre)
- **Build Tool:** [Vite](https://vite.dev)

## Monorepo Structure

Here’s how I’ve organized the project:

/server - Backend server code (Gleam + Wisp)
/client - Frontend client code (Gleam + Lustre + Vite)
/shared - Shared modules between client and server (if any)


## Running the Application

### Server

The server handles all the chess game logic, including game creation and state management. It’s built using Gleam, with Wisp handling the framework and Mist serving as the HTTP webserver.

### Client

The client renders the chessboard and allows users to interact with the game. It’s built using Gleam with Lustre for the frontend and Vite for bundling.

## Contributing

I’m always open to contributions! Whether it’s fixing bugs, adding features, or offering suggestions, feel free to jump in.

Here’s how you can contribute:

1. **Fork** the repository.
2. Create a new branch for your feature: `git checkout -b feature/your-feature`.
3. Push your changes and open a **pull request**. 🚀

## License

This project is licensed under the **MIT License**, meaning you’re free to use, modify, and share it.

---

I hope you enjoy playing and tinkering with this chess project as much as I enjoyed building it! ♟️😊

  