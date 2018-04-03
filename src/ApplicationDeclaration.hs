module ApplicationDeclaration where 

import CoreLib
import AppState
import Control.Monad.State.Lazy
type Application = StateT RootStore IO ()

