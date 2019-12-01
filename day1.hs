import Text.Parsec hiding (parse)
import Util

main = do
  print ""
  return ()

-- parser
type Parser a = Parsec String () a

parse :: String -> Either ParseError _
parse = runParser parser () ""

parser :: Parser _
parser = _
