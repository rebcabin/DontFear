type St   = Int 
data M a  = M (St -> (St, a))

data Term = Con Int 
          | Div Term Term 
          | Add Term Term 
  deriving Show

instance Monad M where
  return v = M (\st -> (st, v))
  M fst0 >>= famb =
    M $ \st0 ->
      let (st1, v1) = fst0 st0
          M fst1    = famb v1
      in  fst1 st1

tick :: M ()
tick =  M (\st -> (st+1, ()))

eval :: Term -> M Int 
eval (Con a) = return a 

eval (Div t u) =
  do r1 <- eval t
     r2 <- eval u
     tick >> return (r1 `div` r2)

answer, error' :: Term 
answer = (Div (Div (Con 1972 ) (Con 2 )) (Con 23 )) 
error' = (Div (Con 1 ) (Con 0 )) 
--------------------------------------------------------
main =
  print 3
-- print 3
--  let M e1 = (eval answer)
--      M e2 = (eval error')
--  in (e1 0, e2 0) 
--------------------------------------------------------

