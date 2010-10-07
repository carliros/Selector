{-# LANGUAGE UndecidableInstances#-}

module ParserHtml where

import ScannerHtml
import UU.Parsing
import Data.Char
import Data.List
import NTree

parseHtml file = do 
    tokens <- scanner file
    ast    <- parseIO pHtml tokens
    return ast

pHtml =  (\hd bd -> NTree (NTag "html") [hd, bd]) <$ pOpen "html" <*> pHead <*> pBody <* pClose "html"

pHead = (\stl -> NTree (NTag "head") [stl]) <$> pTagged "head" pStyle

pStyle = NTree NStyle <$> pTagged "style" (pList1 pRuleCss)

pRuleCss = (\sel pr -> NTree (NRuleCss (sel,pr)) []) <$> pSelector <* pSymbol "{" 
                                                                        <*> pList1Sep (pSymbol ";") pProperty
                                                                   <* pSymbol "}"

pSelector = SimpSelector  <$> pSSelector
         <|> CombSelector <$> pSSelector <*> pCombinator <*> pSelector

pSSelector = TypeSelector  <$> pString
          <|> UnivSelector <$  pSymbol "*"

pCombinator = (pSymbol ">" <|> pSymbol "+") `opt` " "

pProperty = Property <$> pString <* pSymbol ":" <*> pString

pBody = NTree (NTag "body") <$> pTagged "body" (pList1 pElem)

pElem =  pEHead <|> pParag <|> pBig <|> pSmall <|> pDiv <|> pText

pEHead =  NTree (NTag "h1") <$> pTagged "h1" (pList1 pElem)
      <|> NTree (NTag "h2") <$> pTagged "h2" (pList1 pElem)
      <|> NTree (NTag "h3") <$> pTagged "h3" (pList1 pElem)
      <|> NTree (NTag "h4") <$> pTagged "h4" (pList1 pElem)
      <|> NTree (NTag "h5") <$> pTagged "h5" (pList1 pElem)
      <|> NTree (NTag "h6") <$> pTagged "h6" (pList1 pElem)

pParag = NTree (NTag "p"    ) <$> pTagged "p"     (pList1 pElem)
pBig   = NTree (NTag "big"  ) <$> pTagged "big"   (pList1 pElem)
pSmall = NTree (NTag "small") <$> pTagged "small" (pList1 pElem)
pDiv   = NTree (NTag "div"  ) <$> pTagged "div"   (pList1 pElem)

pText = (\lstr -> NTree (NText (unwords lstr)) []) <$> pList1 pString

pTagged tag p = pOpen tag *> p <* pClose tag
pOpen  str = pKeyword  $ "<"  ++ str ++ ">"
pClose str = pKeyword  $ "</" ++ str ++ ">"

-- Integracion Scanner - Parser

instance Eq Tok => Eq Token where
  (Token TokString      _  _) == (Token TokString      _  _) = True
  (Token TokValue       _ _)  == (Token TokValue       _  _) = True
  (Token t1             s1 _) == (Token t2             s2 _) = t1 == t2 && s1 == s2

instance Ord Token where
  compare x y | x==y      = EQ
              | x<=y      = LT
              | otherwise = GT
  Token tok1 str1 _ <= Token tok2 str2 _
      = tok1 < tok2 || (tok1 == tok2 && str1 <= str2)

instance Symbol Token

tSym :: Tok -> String -> Parser Token String
tSym tk str = obtenerVal <$> pSym (Token tk str 0)

obtenerVal (Token _ v _) = v

toInt df str@(x:xs) 
    = if isDigit x
	  then read str :: Int
      else df

--pText       = pString `opt` ""
pString  = tSym TokString ""
pKeyword = tSym TokKeyword
pSymbol  = tSym TokSymbol
pValue   = tSym TokValue ""

