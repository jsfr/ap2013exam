module SalsaParser(parseString, parseFile, Error) where

import SalsaAst
import SimpleParse
import Data.Char
import Control.Monad(replicateM)

type Error = String

spaces1 :: Parser String
spaces1 = many1 space

token1 :: Parser a -> Parser a
token1 p = spaces1 >> p

parens :: Parser a -> Parser a
parens p = do symbol "("
              x <- p
              symbol ")"
              return x

braces :: Parser a -> Parser a
braces p = do symbol "{"
              x <- p
              symbol "}"
              return x

constituent :: Parser Char
constituent = satisfy isAlphaNum <|> char '_'

sident :: Parser Ident
sident = do c <- satisfy isLower
            cs <- many constituent
            if (c:cs) `notElem` reserved
                then return (c:cs)
                else reject
    where reserved = ["viewdef", "rectangle", "circle", "group", "view",
                       "blue", "plum", "red", "green", "orange"]

vident :: Parser Ident
vident = do c <- satisfy isUpper
            cs <- many constituent
            return (c:cs)

colour :: Parser Colour
colour = blue <|> plum <|> red <|> green <|> orange
    where blue   = symbol "blue"   >> return Blue
          plum   = symbol "plum"   >> return Plum
          red    = symbol "red"    >> return Red
          green  = symbol "green"  >> return Green
          orange = symbol "orange" >> return Orange

prim :: Parser Expr
prim = integer <|> proj <|> parens expr
    where integer = do s <- many1 (satisfy isDigit)
                       return $ Const (read s :: Integer)
          proj = do sid <- sident
                    symbol "."
                    plane <- satisfy (`elem` "xy")
                    case plane of
                        'x' -> return $ Xproj sid
                        'y' -> return $ Yproj sid
                        _ -> reject


expr :: Parser Expr
expr = prim `chainl1` (plusop <|> minusop)
    where plusop = symbol "+" >> spaces >> return Plus
          minusop = symbol "-" >> spaces >> return Minus

pos :: Parser Pos
pos = relParse <|> absParse
    where pos' = do e1 <- spaces >> expr
                    symbol ","
                    e2 <- spaces >> expr
                    return (e1, e2)
          relParse = do symbol "+"
                        (e1, e2) <- parens pos'
                        return $ Rel e1 e2
          absParse = do (e1, e2) <- parens pos'
                        return $ Abs e1 e2

command :: Parser Command
command = par
    where par = at `chainl1` parop
          parop = symbol "||" >> return Par
          at = bop At (move <|> braces command) (symbol "@") (spaces >> vident)
          move = do sids0 <- token sident
                    sids1 <- many (token1 sident)
                    symbol "->"
                    p <- pos
                    return $ Move (sids0:sids1) p
          bop f l op r = do x <- l
                            bop' x
              where bop' x = do op
                                y <- r
                                bop' $ f x y
                            <|> return x

definition :: Parser Definition
definition = viewdef <|> rectangle <|> circle <|> view <|> group
    where exprn n = replicateM n (spaces1 >> expr)
          viewdef = do string "viewdef"
                       vid <- token1 vident
                       [e1, e2] <- exprn 2
                       return $ Viewdef vid e1 e2
          rectangle = do string "rectangle"
                         sid <- token1 sident
                         [e1, e2, e3, e4] <- exprn 4
                         col <- colour
                         return $ Rectangle sid e1 e2 e3 e4 col
          circle =  do string "circle"
                       sid <- spaces1 >> sident
                       [e1, e2, e3] <- exprn 3
                       col <- colour
                       return $ Circle sid e1 e2 e3 col
          view = do string "view"
                    vid <- token1 vident
                    return $ View vid
          group = do string "group"
                     vid <- token1 vident
                     symbol "["
                     vids <- token (vident `sepBy1` spaces1)
                     symbol "]"
                     return $ Group vid vids

defcom :: Parser DefCom
defcom = comParse <|> defParse
    where comParse = do com <- command
                        return $ Com com
          defParse = do def <- definition
                        return $ Def def

program :: Parser Program
program = spaces >> defcom `sepBy1` spaces1

parseString :: String -> Either Error Program
parseString s =
  case parse' (do {e <- program; token eof; return e}) s of
      (e:_) -> Right e
      []    -> Left "Parser Error: Couldn't parse"

parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename
