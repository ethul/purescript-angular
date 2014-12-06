module TodomvcF.TodomvcF
  ( Todo(..)
  , Todomvc()
  , TodomvcF(..)
  , add
  , remove
  , edit
  , revert
  , all
  , clearCompleted
  , markAll
  , completed
  , doneEdit
  ) where

import Control.Monad.Free (FreeC(), liftFC)
import Data.Foreign.Class (IsForeign, read, readProp)

newtype Todo = Todo { title :: String , completed :: Boolean }

type Todomvc a = FreeC TodomvcF a

data TodomvcF a
  = Add Todo a
  | Remove Todo a
  | Edit Todo a
  | Revert Todo a
  | All ([Todo] -> a)
  | ClearCompleted a
  | MarkAll Boolean a
  | Completed Todo a
  | DoneEdit Todo a

instance todoIsForeign :: IsForeign Todo where
  read a = do
    title <- readProp "title" a
    completed <- readProp "completed" a
    return $ Todo { title: title
                  , completed: completed }

add :: Todo -> Todomvc Unit
add t = liftFC $ Add t unit

remove :: Todo -> Todomvc Unit
remove t = liftFC $ Remove t unit

edit :: Todo -> Todomvc Unit
edit t = liftFC $ Edit t unit

doneEdit :: Todo -> Todomvc Unit
doneEdit t = liftFC $ DoneEdit t unit

revert :: Todo -> Todomvc Unit
revert t = liftFC $ Revert t unit

all :: Todomvc [Todo]
all = liftFC $ All id

clearCompleted :: Todomvc Unit
clearCompleted = liftFC $ ClearCompleted unit

markAll :: Boolean -> Todomvc Unit
markAll c = liftFC $ MarkAll c unit

completed :: Todo -> Todomvc Unit
completed t = liftFC $ Completed t unit
