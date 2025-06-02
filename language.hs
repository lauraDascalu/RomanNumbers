--syntax
data E = N Int| Plus E E| Mult E E| 
        B Bool| LessThan E E| Or E E deriving (Show, Eq)

zero = N 0
one= N 1
true= B True
false=B False

--expression => expression `terminal` expression
--semantics
    --i: integer values
    --b: boolean values
eval:: E -> Maybe( Either Int Bool)
eval(N i)= Just(Left i)
eval(B b)= Just(Right b)
eval( Plus e1 e2)=
    let r1=eval e1
        r2=eval e2
    in case (r1, r2) of
        (Just(Left i1), Just (Left i2)) -> Just (Left (i1+i2))
        (_,_)->Nothing
eval (Mult e1 e2)=
    let r1=eval e1
        r2=eval e2
    in case (r1, r2) of
        (Just(Left i1), Just(Left i2)) -> Just(Left( i1*i2))
        (_,_)->Nothing
eval (LessThan e1 e2)=
    let r1=eval e1
        r2=eval e2
    in case (r1,r2) of
        (Just(Left r1), Just(Left r2))->
            if r1<r2 
                then Just(Right True)
                else Just(Right False)
        _-> Nothing
eval (Or e1 e2) =
  let r1 = eval e1
      r2 = eval e2
  in case r1 of
       Nothing -> Nothing
       Just (Left _) -> Nothing
       Just (Right True) ->
         case r2 of
           Nothing -> Nothing
           Just (Left _) -> Nothing
           Just (Right True) -> Just (Right True)
           _ -> Just (Right False)
       _ -> Just (Right False)

simplify:: E->E
simplify e@N{}= e
simplify e@B{}= e
simplify (Plus e1 e2)= Plus (simplify e1)(simplify e2)
simplify (Mult (N 0) _)= N 0
simplify (Mult e1 e2)= Mult (simplify e1)(simplify e2)
simplify (LessThan e1 e2)= LessThan (simplify e1)(simplify e2)
simplify (Or e1 e2)= Or (simplify e1)(simplify e2)

--exhaustively apply simplify until n exhaustively further simplification
simplifyFix:: E-> E
simplifyFix e= 
    let e1=simplify e
        in if e1==e then e
            else simplifyFix e1

data Type = TInt | TBool deriving Show

typechecker:: E-> Maybe Type
typechecker (N _)= Just TInt
typechecker (B _)=Just TBool
typechecker (Plus e1 e2)=
    case (typechecker e1, typechecker e2) of
        (Just TInt, Just TInt) -> Just TInt
        (_, _)-> Nothing
typechecker (Mult e1 e2)=
    case (typechecker e1, typechecker e2) of
        (Just TInt, Just TInt)-> Just TInt
        (_,_)-> Nothing
typechecker (LessThan e1 e2) =
     case (typechecker e1, typechecker e2) of
       (Just TInt, Just TInt) -> Just TBool
       (_, _) -> Nothing
typechecker (Or e1 e2) =
     case (typechecker e1, typechecker e2) of
       (Just TBool, Just TBool) -> Just TBool
       (_, _) -> Nothing

--for well-typed expression, it is possible to optimize the evaluator
--no need to cover cases that involve Just and Nothing
evalWellType:: E->Either Int Bool
evalWellType (N i)= Left i
evalWellType (B b)= Right b
evalWellType (Plus e1 e2)=
    case (evalWellType e1, evalWellType e2) of
        (Left i1, Left i2)-> Left (i1+i2)
evalWellType (Mult e1 e2)=
    case (evalWellType e1, evalWellType e2) of
        (Left i1, Left i2) -> Left (i1*i2)
evalWellType (LessThan e1 e2)=
    case (evalWellType e1, evalWellType e2) of
        (Left i1, Left i2)-> Right( i1< i2)
evalWellType (Or e1 e2)=
    case (evalWellType e1) of
        Right True -> Right True
        _          -> evalWellType e2

--enforcing well-typing of expressions when builidng ASTs using GADTs
data EE a where
    N_EE:: Int -> EE Int
    B_EE:: Bool -> EE Bool
    Plus_EE:: EE Int-> EE Int -> EE Int
    Mult_EE:: EE Int-> EE Int-> EE Int
    LessThan_EE:: EE Int-> EE Int-> EE Bool
    Or_EE:: EE Bool-> EE Bool -> EE Bool

--redefine the evaluator for GADTs
evalEE:: EE a-> a
evalEE (N_EE i)= i
evalEE (B_EE b) = b
evalEE (Plus_EE e1 e2) = evalEE e1 + evalEE e2
evalEE (Mult_EE e1 e2) = evalEE e1 * evalEE e2
evalEE (LessThan_EE e1 e2)
  | evalEE e1 < evalEE e2 = True
  | otherwise             = False
evalEE (Or_EE e1 e2) = evalEE e1 || evalEE e2

evaluateExpr :: E -> IO()
evaluateExpr expr= do
    print( "Expression: "++ show expr)   
    let simpl_expr= simplifyFix expr
    case typechecker simpl_expr of
        Just _ -> print("Evaluation (well-typed): "++ show (evalWellType simpl_expr))
        Nothing-> print("Evaluation (not well-typed): "++ show (eval simpl_expr))
    putStrLn ""

main::IO()
main=do
    let expr1= Plus (N 3) (Mult (N 2)(N 4)) -- 3+ (2*4)
        expr2 = LessThan (N 5) (Plus (N 2) (N 2))  -- 5 < (2 + 2)
        expr3 = Or (B False) (B True)  -- False || True
        expr4 = Plus (N 5)(B True)

    evaluateExpr expr1
    evaluateExpr expr2
    evaluateExpr expr3
    evaluateExpr expr4