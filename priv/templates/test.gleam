import {{name}}
import expect

pub fn hello_world_test() {
  {{name}}:hello_world()
  |> expect:equal(_, "Hello, from {{name}}!")
}
