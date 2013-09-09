/*-------------------------------------------------------------------.
| Types, variables and functions do not share the same environment.  |
| In addition, there can be different nesting levels.  Below there   |
| are four different `a's.                                           |
`-------------------------------------------------------------------*/

let
   type a = int
   var a : a := 2
   function a (a : a) : a = a
in
   a (a + a)
end
