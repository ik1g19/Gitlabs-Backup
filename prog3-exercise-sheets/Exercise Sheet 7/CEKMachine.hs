data Expr = Var String | Lam String Expr | App Expr Expr | Cl (String Expr,Env) deriving (Eq,Show,Read)
type Env  = [(String,Expr)]

lookupVar :: String -> Env -> Maybe Expr
lookupVar var env | lookup var env /= Nothing = 