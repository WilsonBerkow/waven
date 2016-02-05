module ContextZipper where

type ContextZipper e =
    LeftEnd (e, e) (List e) -- (focus, rightside), rest
  | MidElem (List e) (e, e, e) (List e) -- many, (leftside, focus, rightside), rest
  | RightEnd (List e) (e, e) -- many, (leftside, focus)

mk: List x -> ContextZipper x
mk l =
  case l of
    x :: y :: zs -> LeftEnd (x, y) zs
    _ -> Debug.crash "Tried to make a zipper from an empty list"

toList : ContextZipper x -> List x
toList cz =
  case cz of
    LeftEnd (f, r) rest -> f :: r :: rest
    MidElem prev (l, f, r) rest -> prev ++ (l :: f :: r :: rest)
    RightEnd prev (l, f) -> prev ++ [l, f]

next : ContextZipper a -> ContextZipper a
next cz =
  case cz of
    LeftEnd (f, r) (x::xs) ->
      MidElem [] (f, r, x) xs

    LeftEnd (f, r) [] ->
      RightEnd [] (f, r)

    MidElem xs (l, f, r) (y::ys) ->
      MidElem (xs ++ [l]) (f, r, y) ys

    MidElem xs (l, f, r) [] ->
      RightEnd (xs ++ [l]) (f, r)

    RightEnd xs (l, f) ->
      Debug.crash "Tried to get ContextZipper.next of RightEnd value"

