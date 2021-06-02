main :: IO()
main = do
  a <- getLine
  let inp = read a :: Int
  let outp1 = inp ^ 2
  let outp2 = inp ^ 3
  print outp1
  print outp2
  return ()