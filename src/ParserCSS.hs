module Parser where

import Text.ParserCombinators.UU
import DataCss
import Data.List

runfile :: String -> IO()
runfile file = do
    str <- readFile file
    let str' = unwords $ words str
    print str'
    run str'

run :: String -> IO ()
run inp = do let r@(a, errors) =  parse ( (,) <$> pCss <*> pEnd) (listToStr inp (0,0))
             putStrLn "--"
             putStrLn ("-- > Result: " ++ show a)
             if null errors then  return ()
                            else  do putStr ("-- > Correcting steps: \n")
                                     show_errors errors
             putStrLn "-- "

parseCss inp = fst $ parse ( (,) <$> pCss <*> pEnd) (listToStr inp (0,0))

--pCss = pList pRule
pCss = pSelector

pRule = (,) <$> pSelector <* pSymbol '{' 
                                <*> pList1Sep (pSymbol ';') pProperty
                          <* pSymbol '}'

pSelector :: Parser Selector
pSelector =  SimpSelector <$> pSSelector
         <|> CombSelector <$> pSSelector <*> pCombinator <*> pSelector

pCombinator :: Parser String
pCombinator =         pSymbol '>'
           <|>        pSymbol '+'
           <|> " " <$ pList1 (pSym ' ')

pSSelector :: Parser SSelector
pSSelector =  TypeSelector <$> pIdentifier
          <|> UnivSelector <$  pSym '*'

pIdentifier :: Parser String
pIdentifier = pList1 (pSym ((\t -> ('a'<=t&&t<='z')||('0'<=t&&t<='9')),"'a'..'z', '0'..'9'", 'a'))

pProperty = pDisplay -- <|> pPosition <|> pTop <|> pRight <|> pBottom <|> pLeft

pDisplay = Property <$> pToken "display" <* pSymbol ':' <*> pDisplayValue
pDisplayValue =  None   <$ pToken "none"
             <|> Inline <$ pToken "inline"
             <|> Block  <$ pToken "block"

pSymbol :: Char -> Parser String
pSymbol c = [c] <$ pList (pSym ' ') <* pSym c <* pList (pSym ' ')

--pNothing = pList (pAnySym " \r\t\n")

