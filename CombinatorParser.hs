module CombinatorParser where

  newtype Parser t = Parser (String -> [(t,String)])
  run (Parser p) = p

  char s =
    Parser (\inp -> case inp of
      (x:xs) | s == x -> [(x,xs)]
      otherwise       -> [])

  oneOf xx =
    Parser (\inp -> case inp of
      (s:ss) | elem s xx -> [(s,ss)]
      otherwise          -> [])

  sat pred =
    Parser (\inp -> case inp of
      (s:ss) | pred s -> [(s,ss)]
      otherwise       -> [])

  digit = sat (\x -> x >= '0' && x<= '9')
  