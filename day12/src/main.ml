module Main = Runner.Make (struct
  let day = 12

  module Answer = Z
  include Day12
end)

let () = Main.run ()
