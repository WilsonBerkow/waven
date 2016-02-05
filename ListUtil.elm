module ListUtil where

iterate : (a -> a) -> a -> Int -> List a
iterate f b n =
  case n of
    0 -> []
    1 -> [b]
    n -> b :: iterate f (f b) (n - 1)

replaceHead new l =
  case l of
    x::xs -> new::xs
    [] -> []

replaceLast new l =
  case l of
    [] -> []
    [_] -> [new]
    x::xs -> x :: replaceLast new xs
