module Application where
type Application = StateT (Int, Int) (StateT Board IO) ()
