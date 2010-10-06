

-- UUAGC 0.9.29 (./src/ag/NTree.ag)
module NTree where
{-# LINE 5 "./src/ag/NTree.ag" #-}

--import DataCss
import Data.List
import Data.Maybe
import FSTree
{-# LINE 12 "./src/ag/NTree.hs" #-}
{-# LINE 39 "./src/ag/NTree.ag" #-}

defaultStyle :: [Selector]
defaultStyle = [ --SimpSelector (TypeSelector "big")
                 CombSelector (TypeSelector "body") ">" (CombSelector (TypeSelector "p") " " (SimpSelector (TypeSelector "big")))
               , SimpSelector (TypeSelector "small")
               --, SimpSelector UnivSelector
               ]
{-# LINE 21 "./src/ag/NTree.hs" #-}

{-# LINE 94 "./src/ag/NTree.ag" #-}

applySelector :: Node -> [Node] -> [Node] -> Selector -> Bool
applySelector nd _ _ (SimpSelector s) = case s of
                                            TypeSelector nm -> nd == (NTag nm)
                                            UnivSelector    -> True
applySelector nd fths sbls (CombSelector ssel op sel) = case op of
                                                            " " -> (verifyDescd ssel fths) && (applySelector nd fths sbls sel) -- descendant selector
                                                            ">" -> (verifyChild ssel fths) && (applySelector nd fths sbls sel) -- child selector
                                                            "+" -> (verifySibln ssel sbls) && (applySelector nd fths sbls sel) -- sibling selector
    where verifyDescd s fths = case s of
                                TypeSelector nm -> elem (NTag nm) fths
                                UnivSelector    -> True -- fix me: what if i am the root, who is my father?, of course the fhts will be empty
          verifyChild s []     = False
          verifyChild s (x:xs) = case s of
                                    TypeSelector nm -> (NTag nm) == x
                                    UnivSelector    -> True -- fix me: what if i am the root, who is my father?, of course the fhts will be empty
          verifySibln s sbls = case s of
                                TypeSelector nm -> elem (NTag nm) sbls
                                UnivSelector    -> True -- fix me: what if i am the first child, who is my left sibling?, of course the sbls will be empty
{-# LINE 43 "./src/ag/NTree.hs" #-}

{-# LINE 30 "./src/ag//DataTree.ag" #-}

instance Eq Node where
    NTag n1 == NTag n2 = n1 == n2
    _      == _      = False
{-# LINE 50 "./src/ag/NTree.hs" #-}

{-# LINE 36 "./src/ag//DataTree.ag" #-}

-- examples
ex1 = NTree (NTag "html") [ NTree (NTag "h1") [NTree (NText "title h1" ) [], NTree (NTag "em") [NTree (NText "element em") []]]
                          , NTree (NTag "p" ) [NTree (NTag "small") [NTree (NText "small"  ) []], NTree (NText "element p") [], NTree (NTag "small") [NTree (NText "small"  ) []]]
                          , NTree (NText "carlos") []
                          , NTree (NTag "em") [NTree (NText "element p") [], NTree (NTag "small") [NTree (NText "small"  ) []]]
                          ]

ex2 = NTree (NTag "html") [NTree (NTag "div") [ NTree (NTag "h1")[ NTree (NText "text")[]] 
                                              , NTree (NTag "p") [ NTree (NTag "small")[], NTree (NText "text") [], NTree (NTag "big") []]
                                              ]
                          ]

ex3 = NTree (NTag "html") [NTree (NTag "head")[], NTree (NTag "body") []]

ex4 = NTree (NTag "html") [NTree (NTag "head") [NTree (NTag "body")[]]]

ex5 = NTree (NTag "html") []
{-# LINE 71 "./src/ag/NTree.hs" #-}
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
                ( Node ,(Maybe BoxTree),([Maybe Selector]))
sem_NTree_NTree :: T_Node  ->
                   T_NTrees  ->
                   T_NTree 
sem_NTree_NTree node_ ntrees_  =
    (\ _lhsIcss
       _lhsIfths ->
         (let _lhsOnode :: Node 
              _ntreesOsbls :: ([Node])
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: (Maybe BoxTree)
              _ntreesOcss :: ([Selector])
              _ntreesOfths :: ([Node])
              _nodeInd :: Node 
              _ntreesIbefs :: ([Node])
              _ntreesIres :: ([Maybe BoxTree])
              _ntreesIsel :: ([Maybe Selector])
              _lhsOnode =
                  ({-# LINE 17 "./src/ag/NTree.ag" #-}
                   _nodeInd
                   {-# LINE 103 "./src/ag/NTree.hs" #-})
              _ntreesOsbls =
                  ({-# LINE 34 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 107 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 51 "./src/ag/NTree.ag" #-}
                   let nsel = case _nodeInd of
                                 NTag _         -> Nothing
                                 NText _        -> Nothing
                                 NStyle         -> Nothing
                                 NRuleCss (s,p) -> Just s
                   in nsel : _ntreesIsel
                   {-# LINE 116 "./src/ag/NTree.hs" #-})
              _rlist =
                  ({-# LINE 70 "./src/ag/NTree.ag" #-}
                   map (applySelector _nodeInd _lhsIfths _ntreesIbefs) _lhsIcss
                   {-# LINE 120 "./src/ag/NTree.hs" #-})
              _sel =
                  ({-# LINE 71 "./src/ag/NTree.ag" #-}
                   or _rlist
                   {-# LINE 124 "./src/ag/NTree.hs" #-})
              _num =
                  ({-# LINE 72 "./src/ag/NTree.ag" #-}
                   if _sel     then fromMaybe (-1) (elemIndex True _rlist    ) + 1 else 0
                   {-# LINE 128 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 81 "./src/ag/NTree.ag" #-}
                   let nd = case _nodeInd of
                             NTag  str  -> Just $ BoxTag  _sel     _num     str
                             NText str  -> Just $ BoxText _sel     _num     str
                             NStyle     -> Nothing
                             NRuleCss _ -> Nothing
                   in if isNothing nd
                      then Nothing
                      else Just $ BoxTree (fromJust nd) (catMaybes _ntreesIres)
                   {-# LINE 139 "./src/ag/NTree.hs" #-})
              _ntreesOcss =
                  ({-# LINE 63 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 143 "./src/ag/NTree.hs" #-})
              _ntreesOfths =
                  ({-# LINE 19 "./src/ag/NTree.ag" #-}
                   _lhsIfths
                   {-# LINE 147 "./src/ag/NTree.hs" #-})
              ( _nodeInd) =
                  (node_ )
              ( _ntreesIbefs,_ntreesIres,_ntreesIsel) =
                  (ntrees_ _ntreesOcss _ntreesOfths _ntreesOsbls )
          in  ( _lhsOnode,_lhsOres,_lhsOsel)))
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
                 ( ([Node]),([Maybe BoxTree]),([Maybe Selector]))
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
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: ([Maybe BoxTree])
              _hdOcss :: ([Selector])
              _tlOcss :: ([Selector])
              _hdInode :: Node 
              _hdIres :: (Maybe BoxTree)
              _hdIsel :: ([Maybe Selector])
              _tlIbefs :: ([Node])
              _tlIres :: ([Maybe BoxTree])
              _tlIsel :: ([Maybe Selector])
              _hdOfths =
                  ({-# LINE 21 "./src/ag/NTree.ag" #-}
                   _hdInode : _lhsIfths
                   {-# LINE 189 "./src/ag/NTree.hs" #-})
              _tlOfths =
                  ({-# LINE 22 "./src/ag/NTree.ag" #-}
                   _lhsIfths
                   {-# LINE 193 "./src/ag/NTree.hs" #-})
              _tlOsbls =
                  ({-# LINE 29 "./src/ag/NTree.ag" #-}
                   _hdInode : _lhsIsbls
                   {-# LINE 197 "./src/ag/NTree.hs" #-})
              _lhsObefs =
                  ({-# LINE 30 "./src/ag/NTree.ag" #-}
                   _lhsIsbls
                   {-# LINE 201 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 59 "./src/ag/NTree.ag" #-}
                   _hdIsel ++ _tlIsel
                   {-# LINE 205 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 76 "./src/ag/NTree.ag" #-}
                   _hdIres : _tlIres
                   {-# LINE 209 "./src/ag/NTree.hs" #-})
              _hdOcss =
                  ({-# LINE 63 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 213 "./src/ag/NTree.hs" #-})
              _tlOcss =
                  ({-# LINE 63 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 217 "./src/ag/NTree.hs" #-})
              ( _hdInode,_hdIres,_hdIsel) =
                  (hd_ _hdOcss _hdOfths )
              ( _tlIbefs,_tlIres,_tlIsel) =
                  (tl_ _tlOcss _tlOfths _tlOsbls )
          in  ( _lhsObefs,_lhsOres,_lhsOsel)))
sem_NTrees_Nil :: T_NTrees 
sem_NTrees_Nil  =
    (\ _lhsIcss
       _lhsIfths
       _lhsIsbls ->
         (let _lhsObefs :: ([Node])
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: ([Maybe BoxTree])
              _lhsObefs =
                  ({-# LINE 31 "./src/ag/NTree.ag" #-}
                   _lhsIsbls
                   {-# LINE 234 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 60 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 238 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 77 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 242 "./src/ag/NTree.hs" #-})
          in  ( _lhsObefs,_lhsOres,_lhsOsel)))
-- Node --------------------------------------------------------
data Node  = NRuleCss (((Selector, [Property]))) 
           | NStyle 
           | NTag (String) 
           | NText (String) 
           deriving ( Show)
-- cata
sem_Node :: Node  ->
            T_Node 
sem_Node (NRuleCss _rule )  =
    (sem_Node_NRuleCss _rule )
sem_Node (NStyle )  =
    (sem_Node_NStyle )
sem_Node (NTag _string )  =
    (sem_Node_NTag _string )
sem_Node (NText _string )  =
    (sem_Node_NText _string )
-- semantic domain
type T_Node  = ( Node )
sem_Node_NRuleCss :: ((Selector, [Property])) ->
                     T_Node 
sem_Node_NRuleCss rule_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NRuleCss rule_
              {-# LINE 270 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 274 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NStyle :: T_Node 
sem_Node_NStyle  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NStyle
              {-# LINE 282 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 286 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NTag :: String ->
                 T_Node 
sem_Node_NTag string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NTag string_
              {-# LINE 295 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 299 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NText :: String ->
                  T_Node 
sem_Node_NText string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NText string_
              {-# LINE 308 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 312 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
-- Property ----------------------------------------------------
data Property  = Property (String) (String) 
               deriving ( Show)
-- cata
sem_Property :: Property  ->
                T_Property 
sem_Property (Property _prop _value )  =
    (sem_Property_Property _prop _value )
-- semantic domain
type T_Property  = ( Property )
sem_Property_Property :: String ->
                         String ->
                         T_Property 
sem_Property_Property prop_ value_  =
    (let _lhsOnd :: Property 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              Property prop_ value_
              {-# LINE 332 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 336 "./src/ag/NTree.hs" #-})
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
         _ntreeIres :: (Maybe BoxTree)
         _ntreeIsel :: ([Maybe Selector])
         _ntreeOfths =
             ({-# LINE 25 "./src/ag/NTree.ag" #-}
              []
              {-# LINE 360 "./src/ag/NTree.hs" #-})
         _ntreeOcss =
             ({-# LINE 65 "./src/ag/NTree.ag" #-}
              catMaybes _ntreeIsel
              {-# LINE 364 "./src/ag/NTree.hs" #-})
         _lhsOres =
             ({-# LINE 92 "./src/ag/NTree.ag" #-}
              fromJust _ntreeIres
              {-# LINE 368 "./src/ag/NTree.hs" #-})
         ( _ntreeInode,_ntreeIres,_ntreeIsel) =
             (ntree_ _ntreeOcss _ntreeOfths )
     in  ( _lhsOres))
-- SSelector ---------------------------------------------------
data SSelector  = TypeSelector (String) 
                | UnivSelector 
                deriving ( Show)
-- cata
sem_SSelector :: SSelector  ->
                 T_SSelector 
sem_SSelector (TypeSelector _string )  =
    (sem_SSelector_TypeSelector _string )
sem_SSelector (UnivSelector )  =
    (sem_SSelector_UnivSelector )
-- semantic domain
type T_SSelector  = ( SSelector )
sem_SSelector_TypeSelector :: String ->
                              T_SSelector 
sem_SSelector_TypeSelector string_  =
    (let _lhsOnd :: SSelector 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              TypeSelector string_
              {-# LINE 392 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 396 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_SSelector_UnivSelector :: T_SSelector 
sem_SSelector_UnivSelector  =
    (let _lhsOnd :: SSelector 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              UnivSelector
              {-# LINE 404 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 408 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
-- Selector ----------------------------------------------------
data Selector  = CombSelector (SSelector ) (String) (Selector ) 
               | SimpSelector (SSelector ) 
               deriving ( Show)
-- cata
sem_Selector :: Selector  ->
                T_Selector 
sem_Selector (CombSelector _sSelector _string _selector )  =
    (sem_Selector_CombSelector (sem_SSelector _sSelector ) _string (sem_Selector _selector ) )
sem_Selector (SimpSelector _sSelector )  =
    (sem_Selector_SimpSelector (sem_SSelector _sSelector ) )
-- semantic domain
type T_Selector  = ( Selector )
sem_Selector_CombSelector :: T_SSelector  ->
                             String ->
                             T_Selector  ->
                             T_Selector 
sem_Selector_CombSelector sSelector_ string_ selector_  =
    (let _lhsOnd :: Selector 
         _sSelectorInd :: SSelector 
         _selectorInd :: Selector 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              CombSelector _sSelectorInd string_ _selectorInd
              {-# LINE 434 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 438 "./src/ag/NTree.hs" #-})
         ( _sSelectorInd) =
             (sSelector_ )
         ( _selectorInd) =
             (selector_ )
     in  ( _lhsOnd))
sem_Selector_SimpSelector :: T_SSelector  ->
                             T_Selector 
sem_Selector_SimpSelector sSelector_  =
    (let _lhsOnd :: Selector 
         _sSelectorInd :: SSelector 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              SimpSelector _sSelectorInd
              {-# LINE 452 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 456 "./src/ag/NTree.hs" #-})
         ( _sSelectorInd) =
             (sSelector_ )
     in  ( _lhsOnd))