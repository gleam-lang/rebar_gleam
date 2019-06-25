import {{name}}
import gleam/expect

pub fn hello_world_test() {
  {{name}}:hello_world()
  |> expect:equal(_, "Hello, from {{name}}!")
}
