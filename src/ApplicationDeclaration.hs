module ApplicationDeclaration where 

import CoreLib
import Control.Monad.State.Lazy
type Application = StateT (Int, Int) (StateT Board IO) ()

