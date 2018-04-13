module ApplicationDeclaration where 

import ChineseChess.CoreLib
import AppState
import Control.Monad.State.Lazy
type Application = StateT RootStore IO ()

