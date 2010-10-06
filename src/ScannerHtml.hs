module ScannerHtml where
import Data.Char

type Tokens = [Token]

data Token  = Token Tok String NumLin
type NumLin = Int

instance Show Token where
  show (Token t str nl) = show t ++ " " ++ show str ++ " in line " ++ show nl ++ "\n"

data Tok  = TokKeyword      -- <html>, <head>, <body>, <a>, </html>, </head> ... (any tag with)
          | TokSymbol       -- = + > : ; { }
          | TokValue	    -- "attribute value"
          | TokString       -- any string or keystring or ident
          | TokError        -- couldn't reconize
                deriving (Eq, Ord)

instance Show Tok where
  show TokKeyword    = " KeyWord    : "
  show TokSymbol     = " Symbol     : "
  show TokValue	     = " Value      : "
  show TokString     = " String | Ident : "
  show TokError      = " Error      : "
  
keywordstxt  = [ "html", "head", "body", "table", "tr", "td", "th", "caption", "border", "colspan", "rowspan"
               , "h1", "h2", "h3", "h4", "h5", "h6" 
               , "p"
               , "big"
               , "small"
               , "style" ]
keywordsops  = [ "<", ">", "</"]
symbols      = "=+>:;{}";
opchars      = "/<>";

scanner src = do file   <- readFile src
                 tokens <- tokenize keywordstxt keywordsops symbols opchars file
                 return tokens

tokenize ktx kop sbc opc inp = do let tokens = scan ktx kop sbc opc inp 1
                                  return tokens

scan ktx kop sbc opc xs n = scan' xs n
  where scan' []         _ = []
        scan' xxs@(x:xs) n
          = if isSpace x 
            then scan' nbs nn
            else Token tok str n : scan' rs n
            
          where (tok,str,rs)    = token x xs
                (nn,nbs)        = saltarBlancos xxs n
                

        isSymbolChar x	   = x `elem` sbc
        isOperator x       = x `elem` opc
        operatorProcess x xs = let (y,  ys)   = span isOperator xs
                                   (tag, ts)  = span isAlphaNum ys
                                   (send,zs)  = span isOperator ts
                                   sbegin     = x:y
                                   str        = sbegin ++ tag ++ send
                                   in if sbegin `elem` kop && send `elem` kop && tag `elem` ktx
                                      then (TokKeyword, str, zs)
                                      else if isSymbolChar x
                                           then (TokSymbol, [x], xs)
                                           else (TokError, "simbolo desconocido: " ++ str, zs)
        
        isNotComillas x = x /= '\"'

        isText = not.isSeparator

        token x xs
            | isOperator x    = operatorProcess x xs
	        | isSymbolChar x  = (TokSymbol, [x], xs)
            {-
            | isPrint x       = let (ys, zs) = span isText xs
                                    str      = x:ys
                                in (getTokenText str, str, zs)
            -}
	        | x == '\"'       = let (str, (y:ys)) = span isNotComillas xs
			                    in (TokValue, str, ys)
            | isAlphaNum x    = let (ys,zs) = span isAlphaNum xs
                                    str = x:ys
                                in  (TokString, str, zs)
            | otherwise       = (TokError, "simbolo desconocido: " ++ [x], xs)

saltarBlancos []     n = (n,[])
saltarBlancos xxs@(x:xs) n
  | isSpace x = saltarBlancos xs (n + if x == '\n' then 1 else 0)
  | otherwise = (n,xxs)

