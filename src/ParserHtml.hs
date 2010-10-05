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

pHtml =  (\bd -> NTree (Tag "html") [bd]) <$ pOpen "html" <*> pBody <* pClose "html"

--pHead = NTree (Tag "head") <$ pOpen "head" <*> pCss <* pClose "head"

pBody = NTree (Tag "body") <$ pOpen "body" <*> pList1 pElem <* pClose "body"

pElem =  pHead <|> pParag <|> pBig <|> pSmall <|> pText

pHead =  NTree (Tag "h1") <$> pTagged "h1" (pList1 pElem)
     <|> NTree (Tag "h2") <$> pTagged "h2" (pList1 pElem)
     <|> NTree (Tag "h3") <$> pTagged "h3" (pList1 pElem)
     <|> NTree (Tag "h4") <$> pTagged "h4" (pList1 pElem)
     <|> NTree (Tag "h5") <$> pTagged "h5" (pList1 pElem)
     <|> NTree (Tag "h6") <$> pTagged "h6" (pList1 pElem)

pParag = NTree (Tag "p"    ) <$> pTagged "p"     (pList1 pElem)
pBig   = NTree (Tag "big"  ) <$> pTagged "big"   (pList1 pElem)
pSmall = NTree (Tag "small") <$> pTagged "small" (pList1 pElem)

pText = (\lstr -> NTree (Text (unwords lstr)) []) <$> pList1 pString

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
pSep     = tSym TokSeparator

