https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
https://powcoder.com
代写代考加微信 powcoder
Assignment Project Exam Help
Add WeChat powcoder
module FastInterpreter
       ( runProg
       , Error (..)
       )
       where

import FastAST

import Control.Applicative
import Control.Monad
import Data.List
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as Map

-- | Give the printed representation of a value.
printed :: Value -> String
printed (IntValue x) = show x
printed (StringValue s) = s
printed (ReferenceValue ref) = "#<object " ++ show ref ++ ">"
printed (TermValue (Term sym vs)) =
  sym ++ "(" ++ intercalate ", " (map printed vs) ++ ")"

-- ^ Any runtime error.  You may add more constructors to this type
-- (or remove the existing ones) if you want.  Just make sure it is
-- still an instance of 'Show' and 'Eq'.
data Error = Error String
             deriving (Show, Eq)

type Output = String

-- | A key-value store where the keys are of type @k@, and the values
-- are of type @v@.  Used for mapping object references to objects and
-- variable names to values.
type Store k v = Map k v

-- | A mapping from object references to objects.
type GlobalStore = Store ObjectReference ObjectState

-- | A mapping from field names to field values.
type ObjectFields = Store Name Value

-- | A mapping from variable names to variable values.
type MethodVariables = Store Name Value

-- | A mapping from class and method names to methods.
type MethodStore = Store (Name, Name) MethodDecl

-- | The global state of the program execution.
data GlobalState = GlobalState {
    refs :: GlobalStore,
    methods :: MethodStore,
    output :: Output,
    uuid :: ObjectReference
}

init_state :: GlobalState
init_state = GlobalState { refs=Map.empty, methods=Map.empty, output="", uuid=0 }

-- | The state of a single object.
data ObjectState = ObjectState {
    ref :: ObjectReference,
    name :: Name, -- class name
    fields :: ObjectFields
}

init_obj :: ObjectReference -> Name -> ObjectState
init_obj r n = ObjectState { ref=r, name=n, fields=Map.empty }

-- | The state of a method execution.
data MethodState = MethodState {
    vars :: MethodVariables,
    body :: [Expr]
}

init_method :: MethodState
init_method = MethodState { vars=Map.empty, body=[] }

-- | The basic monad in which execution of a Fast program takes place.
-- Maintains the global state, the running output, and whether or not
-- an error has occurred.

data FastM a = FastM {
    runFastM :: Prog -> GlobalState -> Either Error (GlobalState, a)
}

instance Functor FastM where
  fmap = liftM

instance Applicative FastM where
  pure = return
  (<*>) = ap

instance Monad FastM where
    
    -- (>>=) :: FastM a -> (a -> FastM b) -> FastM b
    -- a :: Prog -> GlobalState -> Either Error (GlobalState, a)
    (FastM a) >>= f = do
        FastM $ \p s -> case (a p s) of
                        Right (s', v) -> do (FastM b) <- return $ f v
                                            b p s'
                        Left e -> Left e

    -- return a :: FastM a
    return a = FastM (\_ s -> Right (s,a) )
    
    fail e = error e

getGlobalState :: FastM GlobalState
getGlobalState = FastM (\_ s -> Right (s,s) )

putGlobalState :: GlobalState -> FastM ()
putGlobalState s = FastM (\_ s -> Right (s,()) )

modifyGlobalState :: (GlobalState -> GlobalState) -> FastM ()
modifyGlobalState f = do
    state <- getGlobalState
    FastM (\_ _ -> Right (f state, ()) )

modifyGlobalStore :: (GlobalStore -> GlobalStore) -> FastM ()
modifyGlobalStore f = modifyGlobalState (\s -> s { refs=f (refs s) } )

lookupMethod :: Name -> Name -> FastM MethodDecl
lookupMethod c m = do
    state <- getGlobalState
    case (Map.lookup (c, m) (methods state)) of
        Just method -> FastM (\_ s -> Right(s, method) )
        Nothing -> fail "No such method"

lookupObject :: ObjectReference -> FastM ObjectState
lookupObject ref = do
    state <- getGlobalState
    case (Map.lookup ref (refs state)) of
        Just obj -> FastM (\_ s -> Right (s, obj) )
        Nothing -> fail "No such reference"

setObject :: ObjectReference -> ObjectState -> FastM ()
setObject ref obj =
    modifyGlobalState (\s -> s { refs=Map.insert ref obj (refs s) } )

-- | Add the 'printed' representation of the value to the output.
printValue :: Value -> FastM ()
printValue v = modifyGlobalState (\s -> s { output=out } )
    where out = printed v

-- | Debug messages
debug :: String -> FastM ()
debug s = printValue (StringValue $ "[DEBUG] " ++ s ++ "\n")

-- | Get the program being executed.
askProg :: FastM Prog
askProg = FastM (\p s -> Right (s,p) )

-- | Get a unique, fresh, never-before used object reference for use
-- to identify a new object.
allocUniqID :: FastM ObjectReference
allocUniqID = do
    state <- getGlobalState
    modifyGlobalState (\s -> s { uuid=(uuid state)+1 } )
    return (uuid state)


-- | The monad in which methods (and constructors and receive actions)
-- execute.  Runs on top of 'FastM' - maintains the reference to self,
-- as well as the method variables.
--
-- Note that since FastMethodM runs on top of FastM, a FastMethodM
-- action has access to the global state (through liftFastM).
data FastMethodM a = FastMethodM {
    runFastMethodM :: ObjectReference -> MethodState -> FastM a
}

instance Functor FastMethodM where
  fmap = liftM

instance Applicative FastMethodM where
  pure = return
  (<*>) = ap

instance Monad FastMethodM where

    -- (>>=) :: FastMethodM a -> (a -> FastMethodM b) -> FastMethodM b
    (FastMethodM a) >>= f = do
        prog <- liftFastM $ askProg
        state <- liftFastM $ getGlobalState
        FastMethodM $ \r s -> do
            case (a r s) of
            -- v :: Prog -> GlobalState -> Either Error GlobalState
                FastM v -> do case (v prog state) of
                                   Right (x, v) -> do (FastMethodM b) <- return $ f v
                                                      b r s
    
    -- return a :: FastMethodM a
    return = liftFastM . return
    fail = liftFastM . fail

-- | Perform a 'FastM' operation inside a 'FastMethodM'.
liftFastM :: FastM a -> FastMethodM a
liftFastM = (\op -> FastMethodM (\_ _ -> op))

-- | Who are we?
askSelf :: FastMethodM ObjectReference
askSelf = FastMethodM (\r _ -> return r)

-- | Add the given name-value associations to the variable store.
bindVars :: [(Name, Value)] -> FastMethodM a -> FastMethodM a
bindVars [] s = s
bindVars (v:vs) (FastMethodM g) = do
    FastMethodM $ \r s -> g r (bind v s)
    where bind (n, v) s = s { vars=Map.insert n v (vars s) }

getMethodState :: FastMethodM MethodState
getMethodState = FastMethodM (\_ s -> return s)

putMethodState :: MethodState -> FastMethodM ()
putMethodState s = FastMethodM (\_ s -> return ())

getsMethodState :: (MethodState -> a) -> FastMethodM a
getsMethodState f = do s <- getMethodState
                       return $ f s

modifyMethodState :: (MethodState -> MethodState) -> FastMethodM ()
modifyMethodState f = do s <- getMethodState
                         putMethodState $ f s

getObjectState :: FastMethodM ObjectState
getObjectState = do
    state <- liftFastM $ getGlobalState
    FastMethodM (\r _ -> check $ Map.lookup r (refs state) )
    where check v = case v of
                         Just v' -> return v'
                         Nothing -> fail "No such object"

putObjectState :: ObjectState -> FastMethodM ()
putObjectState obj = do
    state <- liftFastM $ getGlobalState
    liftFastM $ modifyGlobalStore (\s -> Map.insert (ref obj) obj s)

getsObjectState :: (ObjectState -> a) -> FastMethodM a
getsObjectState f = do s <- getObjectState
                       return $ f s

modifyObjectState :: (ObjectState -> ObjectState) -> FastMethodM ()
modifyObjectState f = do s <- getObjectState
                         putObjectState $ f s

-- | Evaluate a method body - the passed arguments are the object in
-- which to run, the initial variable bindings (probably the
-- parameters of the method, constructor or receive action), and the
-- body.  Returns a value and the new state of the object.
evalMethodBody :: ObjectReference
               -> [(Name, Value)]
               -> Exprs
               -> FastM (Value, ObjectState)
evalMethodBody ref vars exprs
    = let (FastMethodM f) = bindVars vars $ eval exprs
      in do
        debug ("Evaluating method")
        f ref init_method
    where eval exps = do
                        val <- evalExprs exps
                        state <- getObjectState
                        return $ (val, state)

evalExprs :: [Expr] -> FastMethodM Value
evalExprs [] = return $ TermValue $ Term "nil" []
evalExprs [e] = evalExpr e
evalExprs (e:es) = evalExpr e >> evalExprs es

evalArgs :: [Expr] -> FastMethodM [Value]
evalArgs exprs = mapM evalExpr exprs

evalExpr :: Expr -> FastMethodM Value
evalExpr (IntConst v)           = return $ IntValue v
evalExpr (StringConst v)        = return $ StringValue v

-- arithmetic
evalExpr (Plus (IntConst lhs) (IntConst rhs)) =
    return $ (IntValue (lhs + rhs))
evalExpr (Minus lhs rhs)        = undefined
evalExpr (Times lhs rhs)        = undefined
evalExpr (DividedBy lhs rhs)    = undefined


evalExpr (TermLiteral n exps)   = undefined
evalExpr (CallMethod e n exps)  = do
    s <- getObjectState
    method <- liftFastM $ lookupMethod (name s) n
    as <- evalArgs exps
    rec <- evalExpr e -- recipient (class)
    (msg, state) <- liftFastM $ evalMethodBody (ref s) (args method as) (body method)
    liftFastM $ sendMessageTo rec msg
    where args m vs = zip (params m) vs
          params m = methodParameters m
          body m = methodBody m
    
evalExpr (SendMessage obj e)    = do
    rec <- evalExpr obj
    msg <- evalExpr e
    liftFastM $ sendMessageTo rec msg

evalExpr (New name exprs)       = undefined --createObject name exprs

-- | Find the declaration of the class with the given name, or cause
-- an error if that name is not a class.
findClassDecl :: Name -> FastM ClassDecl
findClassDecl n = do
    prog <- askProg
    case (filter condition prog) of
        [] -> fail $ "No class definition of class " ++ n
        [c] -> (do mapM (register n) (classMethods c); return c)
        _ -> fail $ "Duplicate definitions of class " ++ n
    where condition ClassDecl { className=name } = name == n
          register c m = let (NamedMethodDecl n mtd) = m
                         in modifyGlobalState (\s -> s { methods=Map.insert (c, n) mtd (methods s)} )

-- | Instantiate the class with the given name, passing the given
-- values to the constructor.
createObject :: Name -> [Value] -> FastM ObjectReference
createObject name args = do
    id <- allocUniqID
    
    debug ("Creating class " ++ name)
    
    -- FIX: add the object, putObjectState (init_obj id name)
    
    ClassDecl { classConstructor=constructor
              , classMethods=methods
              , classReceive=receive } <- findClassDecl name
    
    case constructor of
                Just MethodDecl { methodParameters=p
                                , methodBody=b } -> run id (zip p args) b
                Nothing -> run id [] []
    return id
    
    where run id args body = return id --evalMethodBody id args body
          reg id dec = let (NamedMethodDecl name method) = dec
                       in modifyObjectState (\s -> s { fields=Map.insert name (v method) (fields s) } )
                       where v m = StringValue ""

sendMessageTo :: Value -> Value -> FastM Value
sendMessageTo = undefined

runProg :: Prog -> Either Error String
runProg p = let (FastM f) = createObject "Main" []
            in case fmap fst $ f p init_state of
                    Right state -> Right $ output state
                    Left e -> Left e
    -- f :: Prog -> GlobalState -> Either Error (GlobalState, ClassDecl)

