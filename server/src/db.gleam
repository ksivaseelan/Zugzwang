import gleam/http
import gleam/http/request
import gleam/http/response
import gleam/httpc
import gleam/io
import gleam/json
import gleam/result

pub fn main() {
  // Prepare a HTTP request record
  let assert Ok(base_req) =
    request.to("https://main-ksivaseelan.turso.io/v2/pipeline")

  let body =
    json.object([
      #(
        "requests",
        json.preprocessed_array([
          json.object([
            #("type", json.string("execute")),
            #(
              "stmt",
              json.object([#("sql", json.string("SELECT * FROM chess_games;"))]),
            ),
          ]),
          json.object([#("type", json.string("close"))]),
        ]),
      ),
    ])
    |> json.to_string()

  let req =
    request.set_header(base_req, "content-type", "application/json")
    |> request.set_method(http.Post)
    |> request.set_header(
      "authorization",
      "Bearer eyJhbGciOiJFZERTQSIsInR5cCI6IkpXVCJ9.eyJpYXQiOjE3MzI5MTA3NjksImlkIjoiZThmZmM0ZjMtNzE4ZS00M2FhLTgwY2MtZDAwZDg3MmJlZGIzIn0.RH9uzMQSz2-NxzCI4zVckGDuSly3zO-6SJ7-XT0qlTyMvDiEdli63fYhhGnJGQLQz00PxQaxZ5YQRyAABNXtDw",
    )
    |> request.set_body(body)

  // Send the HTTP request to the server
  use resp <- result.try(httpc.send(req))

  io.debug(resp)

  Ok(resp)
}
