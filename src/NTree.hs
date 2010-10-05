

-- UUAGC 0.9.29 (./src/ag/NTree.ag)
module NTree where
{-# LINE 5 "./src/ag/NTree.ag" #-}

import DataCss
import Data.List
import Data.Maybe
import FSTree
{-# LINE 12 "./src/ag/NTree.hs" #-}
{-# LINE 37 "./src/ag/NTree.ag" #-}

defaultStyle :: [Selector]
defaultStyle = [ --SimpSelector (TypeSelector "big")
                 CombSelector (TypeSelector "body") ">" (CombSelector (TypeSelector "p") " " (SimpSelector (TypeSelector "big")))
               , SimpSelector (TypeSelector "small")
               --, SimpSelector UnivSelector
               ]
{-# LINE 21 "./src/ag/NTree.hs" #-}

{-# LINE 70 "./src/ag/NTree.ag" #-}

applySelector :: Node -> [Node] -> [Node] -> Selector -> Bool
--applySelector nd fths sbls sel = verifySelector sel nd fths sbls
applySelector nd _ _ (SimpSelector s) = case s of
                                            TypeSelector nm -> nd == (Tag nm)
                                            UnivSelector    -> True
applySelector nd fths sbls (CombSelector ssel op sel) = case op of
                                                            " " -> (verifyDescd ssel fths) && (applySelector nd fths sbls sel) -- descendant selector
                                                            ">" -> (verifyChild ssel fths) && (applySelector nd fths sbls sel) -- child selector
                                                            "+" -> (verifySibln ssel sbls) && (applySelector nd fths sbls sel) -- sibling selector
    where verifyDescd s fths = case s of
                                TypeSelector nm -> elem (Tag nm) fths
                                UnivSelector    -> True -- fix me: what if i am the root, who is my father?, of course the fhts will be empty
          verifyChild s []     = False
          verifyChild s (x:xs) = case s of
                                    TypeSelector nm -> (Tag nm) == x
                                    UnivSelector    -> True -- fix me: what if i am the root, who is my father?, of course the fhts will be empty
          verifySibln s sbls = case s of
                                TypeSelector nm -> elem (Tag nm) sbls
                                UnivSelector    -> True -- fix me: what if i am the first child, who is my left sibling?, of course the sbls will be empty
{-# LINE 44 "./src/ag/NTree.hs" #-}

{-# LINE 17 "./src/ag//DataTree.ag" #-}

instance Eq Node where
    Tag n1 == Tag n2 = n1 == n2
    _      == _      = False
{-# LINE 51 "./src/ag/NTree.hs" #-}

{-# LINE 23 "./src/ag//DataTree.ag" #-}

-- examples
ex1 = NTree (Tag "html") [ NTree (Tag "h1") [NTree (Text "title h1" ) [], NTree (Tag "em") [NTree (Text "element em") []]]
                         , NTree (Tag "p" ) [NTree (Tag "small") [NTree (Text "small"  ) []], NTree (Text "element p") [], NTree (Tag "small") [NTree (Text "small"  ) []]]
                         , NTree (Text "carlos") []
                         , NTree (Tag "em") [NTree (Text "element p") [], NTree (Tag "small") [NTree (Text "small"  ) []]]
                         ]

ex2 = NTree (Tag "html") [NTree (Tag "div") [ NTree (Tag "h1")[ NTree (Text "text")[]] 
                                            , NTree (Tag "p") [ NTree (Tag "small")[], NTree (Text "text") [], NTree (Tag "big") []]
                                            ]
                         ]

ex3 = NTree (Tag "html") [NTree (Tag "head")[], NTree (Tag "body") []]

ex4 = NTree (Tag "html")[NTree (Tag "head") [NTree (Tag "body")[]]]

ex5 = NTree (Tag "html") []
{-# LINE 72 "./src/ag/NTree.hs" #-}
-- NTree -------------------------------------------------------
data NTree  = NTree (Node ) (NTrees ) 
            deriving ( Show)
-- cata
sem_NTree :: NTree  ->
             T_NTree 
sem_NTree (NTree _node _ntrees )  =
    (sem_NTree_NTree (sem_Node _node ) (sem_NTrees _ntrees ) )
-- semantic domain
type T_NTree  = ([Selector]) ->
                ([Node]) ->
                ( Node ,BoxTree)
sem_NTree_NTree :: T_Node  ->
                   T_NTrees  ->
                   T_NTree 
sem_NTree_NTree node_ ntrees_  =
    (\ _lhsIcss
       _lhsIfths ->
         (let _lhsOnode :: Node 
              _ntreesOsbls :: ([Node])
              _lhsOres :: BoxTree
              _ntreesOcss :: ([Selector])
              _ntreesOfths :: ([Node])
              _nodeInd :: Node 
              _ntreesIbefs :: ([Node])
              _ntreesIres :: ([BoxTree])
              _lhsOnode =
                  ({-# LINE 17 "./src/ag/NTree.ag" #-}
                   _nodeInd
                   {-# LINE 102 "./src/ag/NTree.hs" #-})
              _ntreesOsbls =
                  ({-# LINE 34 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 106 "./src/ag/NTree.hs" #-})
              _rlist =
                  ({-# LINE 54 "./src/ag/NTree.ag" #-}
                   map (applySelector _nodeInd _lhsIfths _ntreesIbefs) defaultStyle
                   {-# LINE 110 "./src/ag/NTree.hs" #-})
              _sel =
                  ({-# LINE 55 "./src/ag/NTree.ag" #-}
                   or _rlist
                   {-# LINE 114 "./src/ag/NTree.hs" #-})
              _num =
                  ({-# LINE 56 "./src/ag/NTree.ag" #-}
                   if _sel     then fromMaybe (-1) (elemIndex True _rlist    ) + 1 else 0
                   {-# LINE 118 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 65 "./src/ag/NTree.ag" #-}
                   let nd = case _nodeInd of
                             Tag  str -> BoxTag  _sel     _num     str
                             Text str -> BoxText _sel     _num     str
                   in BoxTree nd _ntreesIres
                   {-# LINE 125 "./src/ag/NTree.hs" #-})
              _ntreesOcss =
                  ({-# LINE 47 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 129 "./src/ag/NTree.hs" #-})
              _ntreesOfths =
                  ({-# LINE 19 "./src/ag/NTree.ag" #-}
                   _lhsIfths
                   {-# LINE 133 "./src/ag/NTree.hs" #-})
              ( _nodeInd) =
                  (node_ )
              ( _ntreesIbefs,_ntreesIres) =
                  (ntrees_ _ntreesOcss _ntreesOfths _ntreesOsbls )
          in  ( _lhsOnode,_lhsOres)))
-- NTrees ------------------------------------------------------
type NTrees  = [NTree ]
-- cata
sem_NTrees :: NTrees  ->
              T_NTrees 
sem_NTrees list  =
    (Prelude.foldr sem_NTrees_Cons sem_NTrees_Nil (Prelude.map sem_NTree list) )
-- semantic domain
type T_NTrees  = ([Selector]) ->
                 ([Node]) ->
                 ([Node]) ->
                 ( ([Node]),([BoxTree]))
sem_NTrees_Cons :: T_NTree  ->
                   T_NTrees  ->
                   T_NTrees 
sem_NTrees_Cons hd_ tl_  =
    (\ _lhsIcss
       _lhsIfths
       _lhsIsbls ->
         (let _hdOfths :: ([Node])
              _tlOfths :: ([Node])
              _tlOsbls :: ([Node])
              _lhsObefs :: ([Node])
              _lhsOres :: ([BoxTree])
              _hdOcss :: ([Selector])
              _tlOcss :: ([Selector])
              _hdInode :: Node 
              _hdIres :: BoxTree
              _tlIbefs :: ([Node])
              _tlIres :: ([BoxTree])
              _hdOfths =
                  ({-# LINE 21 "./src/ag/NTree.ag" #-}
                   _hdInode : _lhsIfths
                   {-# LINE 172 "./src/ag/NTree.hs" #-})
              _tlOfths =
                  ({-# LINE 22 "./src/ag/NTree.ag" #-}
                   _lhsIfths
                   {-# LINE 176 "./src/ag/NTree.hs" #-})
              _tlOsbls =
                  ({-# LINE 29 "./src/ag/NTree.ag" #-}
                   _hdInode : _lhsIsbls
                   {-# LINE 180 "./src/ag/NTree.hs" #-})
              _lhsObefs =
                  ({-# LINE 30 "./src/ag/NTree.ag" #-}
                   _lhsIsbls
                   {-# LINE 184 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 60 "./src/ag/NTree.ag" #-}
                   _hdIres : _tlIres
                   {-# LINE 188 "./src/ag/NTree.hs" #-})
              _hdOcss =
                  ({-# LINE 47 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 192 "./src/ag/NTree.hs" #-})
              _tlOcss =
                  ({-# LINE 47 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 196 "./src/ag/NTree.hs" #-})
              ( _hdInode,_hdIres) =
                  (hd_ _hdOcss _hdOfths )
              ( _tlIbefs,_tlIres) =
                  (tl_ _tlOcss _tlOfths _tlOsbls )
          in  ( _lhsObefs,_lhsOres)))
sem_NTrees_Nil :: T_NTrees 
sem_NTrees_Nil  =
    (\ _lhsIcss
       _lhsIfths
       _lhsIsbls ->
         (let _lhsObefs :: ([Node])
              _lhsOres :: ([BoxTree])
              _lhsObefs =
                  ({-# LINE 31 "./src/ag/NTree.ag" #-}
                   _lhsIsbls
                   {-# LINE 212 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 61 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 216 "./src/ag/NTree.hs" #-})
          in  ( _lhsObefs,_lhsOres)))
-- Node --------------------------------------------------------
data Node  = Tag (String) 
           | Text (String) 
           deriving ( Show)
-- cata
sem_Node :: Node  ->
            T_Node 
sem_Node (Tag _string )  =
    (sem_Node_Tag _string )
sem_Node (Text _string )  =
    (sem_Node_Text _string )
-- semantic domain
type T_Node  = ( Node )
sem_Node_Tag :: String ->
                T_Node 
sem_Node_Tag string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              Tag string_
              {-# LINE 238 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 242 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_Text :: String ->
                 T_Node 
sem_Node_Text string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              Text string_
              {-# LINE 251 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 255 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
-- Root --------------------------------------------------------
data Root  = Root (NTree ) 
           deriving ( Show)
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root _ntree )  =
    (sem_Root_Root (sem_NTree _ntree ) )
-- semantic domain
type T_Root  = ( BoxTree)
sem_Root_Root :: T_NTree  ->
                 T_Root 
sem_Root_Root ntree_  =
    (let _ntreeOfths :: ([Node])
         _ntreeOcss :: ([Selector])
         _lhsOres :: BoxTree
         _ntreeInode :: Node 
         _ntreeIres :: BoxTree
         _ntreeOfths =
             ({-# LINE 25 "./src/ag/NTree.ag" #-}
              []
              {-# LINE 278 "./src/ag/NTree.hs" #-})
         _ntreeOcss =
             ({-# LINE 49 "./src/ag/NTree.ag" #-}
              defaultStyle
              {-# LINE 282 "./src/ag/NTree.hs" #-})
         _lhsOres =
             ({-# LINE 63 "./src/ag/NTree.ag" #-}
              _ntreeIres
              {-# LINE 286 "./src/ag/NTree.hs" #-})
         ( _ntreeInode,_ntreeIres) =
             (ntree_ _ntreeOcss _ntreeOfths )
     in  ( _lhsOres))