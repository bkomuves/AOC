
import Data.List
import Data.String
import System.File

--------------------------------------------------------------------------------

fatal : String -> a
fatal msg = assert_total $ idris_crash msg

--------------------------------------------------------------------------------

readInt : String -> Int
readInt = cast

--------------------------------------------------------------------------------

pairs : List a -> List (a,a)
pairs Nil            = Nil
pairs (_::Nil)       = Nil
pairs (x::xs@(y::_)) = (x,y) :: pairs xs

triples : List a -> List (a,a,a)
triples Nil               = Nil
triples (_::Nil)          = Nil
triples (_::_::Nil)       = Nil
triples (x::xs@(y::z::_)) = (x,y,z) :: triples xs

--------------------------------------------------------------------------------

readLines : String -> IO (List String)
readLines fname = do
  ei <- readFilePage 0 forever fname
  case ei of
    Left err => fatal $ "cannot read input file `" ++ fname ++ "`"
    Right (eof,lines) => pure $ map trim lines

--------------------------------------------------------------------------------
