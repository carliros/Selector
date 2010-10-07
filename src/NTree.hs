

-- UUAGC 0.9.29 (./src/ag/NTree.ag)
module NTree where
{-# LINE 5 "./src/ag/NTree.ag" #-}

--import DataCss
import Data.List
import Data.Maybe
import FSTree
{-# LINE 12 "./src/ag/NTree.hs" #-}
{-# LINE 38 "./src/ag/NTree.ag" #-}

defaultStyle :: [Selector]
defaultStyle = [ SimpSelector (TypeSelector "big")
               , CombSelector (TypeSelector "body") ">" (CombSelector (TypeSelector "p") " " (SimpSelector (TypeSelector "big")))
               , SimpSelector (TypeSelector "small")
               , SimpSelector UnivSelector
               ]
{-# LINE 21 "./src/ag/NTree.hs" #-}

{-# LINE 93 "./src/ag/NTree.ag" #-}

data TSelector = TSimplSelector SSelector
               | TDescdSelector SSelector
               | TChildSelector SSelector
               | TSiblnSelector SSelector

transformSelector (SimpSelector s) 
    = [TSimplSelector s]
transformSelector (CombSelector s op sel)
    = case op of
        " " -> (TDescdSelector s) : transformSelector sel
        ">" -> (TChildSelector s) : transformSelector sel
        "+" -> (TSiblnSelector s) : transformSelector sel

applySelector :: Node -> [Node] -> [Node] -> [TSelector] -> Bool
applySelector    nd      fathers   siblings  []
    = True
applySelector    nd      fathers   siblings  (sel:nextSel)
    = case sel of
        TSimplSelector s -> applySimplSelector nd fathers siblings s nextSel
        TDescdSelector s -> applyDescdSelector nd fathers siblings s nextSel
        TChildSelector s -> applyChildSelector nd fathers siblings s nextSel
        TSiblnSelector s -> applySiblnSelector nd fathers siblings s nextSel

applySimplSelector nd fathers siblings s nextSel
    = if testSimpleSelector s nd
      then applySelector nd fathers siblings nextSel
      else False

applyDescdSelector nd []     siblings _ _
    = False
applyDescdSelector nd (f:fs) siblings s nextSel
    = if testSimpleSelector s f
      then if applySelector nd fs siblings nextSel
           then True
           else applyDescdSelector nd fs siblings s nextSel
      else applyDescdSelector nd fs siblings s nextSel

applyChildSelector nd []     siblings _ _
    = False
applyChildSelector nd (f:fs) siblings s nextSel
    = if testSimpleSelector s f
      then applySelector nd fs siblings nextSel
      else False

applySiblnSelector nd _ [] _ _
    = False 
applySiblnSelector nd fathers siblings s nextSel
    = let ntest = case s of
                    TypeSelector nm -> elem (NTag nm) siblings
                    UnivSelector    -> True
      in if ntest
         then applySelector nd fathers siblings nextSel
         else False

testSimpleSelector s nd = case s of
                            TypeSelector nm -> nd == (NTag nm)
                            UnivSelector    -> True
{-# LINE 82 "./src/ag/NTree.hs" #-}

{-# LINE 30 "./src/ag//DataTree.ag" #-}

instance Eq Node where
    NTag n1 == NTag n2 = n1 == n2
    _      == _      = False
{-# LINE 89 "./src/ag/NTree.hs" #-}

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
{-# LINE 110 "./src/ag/NTree.hs" #-}
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
       _lhsIfathers ->
         (let _lhsOnd :: Node 
              _ntreesOfathers :: ([Node])
              _ntreesOsbls :: ([Node])
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: (Maybe BoxTree)
              _ntreesOcss :: ([Selector])
              _nodeInd :: Node 
              _ntreesIbefs :: ([Node])
              _ntreesIres :: ([Maybe BoxTree])
              _ntreesIsel :: ([Maybe Selector])
              _lhsOnd =
                  ({-# LINE 17 "./src/ag/NTree.ag" #-}
                   _nodeInd
                   {-# LINE 142 "./src/ag/NTree.hs" #-})
              _ntreesOfathers =
                  ({-# LINE 22 "./src/ag/NTree.ag" #-}
                   _nodeInd : _lhsIfathers
                   {-# LINE 146 "./src/ag/NTree.hs" #-})
              _fathers =
                  ({-# LINE 23 "./src/ag/NTree.ag" #-}
                   _lhsIfathers
                   {-# LINE 150 "./src/ag/NTree.hs" #-})
              _ntreesOsbls =
                  ({-# LINE 35 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 154 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 50 "./src/ag/NTree.ag" #-}
                   let nsel = case _nodeInd of
                                 NTag _         -> Nothing
                                 NText _        -> Nothing
                                 NStyle         -> Nothing
                                 NRuleCss (s,p) -> Just s
                   in nsel : _ntreesIsel
                   {-# LINE 163 "./src/ag/NTree.hs" #-})
              _rlist =
                  ({-# LINE 69 "./src/ag/NTree.ag" #-}
                   map (applySelector _nodeInd _fathers     _ntreesIbefs) (map (\sel -> reverse (transformSelector sel)) _lhsIcss)
                   {-# LINE 167 "./src/ag/NTree.hs" #-})
              _sel =
                  ({-# LINE 70 "./src/ag/NTree.ag" #-}
                   or _rlist
                   {-# LINE 171 "./src/ag/NTree.hs" #-})
              _num =
                  ({-# LINE 71 "./src/ag/NTree.ag" #-}
                   if _sel     then fromMaybe (-1) (elemIndex True _rlist    ) + 1 else 0
                   {-# LINE 175 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 80 "./src/ag/NTree.ag" #-}
                   let nd = case _nodeInd of
                             NTag  str  -> Just $ BoxTag  _sel     _num     str
                             NText str  -> Just $ BoxText _sel     _num     str
                             NStyle     -> Nothing
                             NRuleCss _ -> Nothing
                   in if isNothing nd
                      then Nothing
                      else Just $ BoxTree (fromJust nd) (catMaybes _ntreesIres)
                   {-# LINE 186 "./src/ag/NTree.hs" #-})
              _ntreesOcss =
                  ({-# LINE 62 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 190 "./src/ag/NTree.hs" #-})
              ( _nodeInd) =
                  (node_ )
              ( _ntreesIbefs,_ntreesIres,_ntreesIsel) =
                  (ntrees_ _ntreesOcss _ntreesOfathers _ntreesOsbls )
          in  ( _lhsOnd,_lhsOres,_lhsOsel)))
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
       _lhsIfathers
       _lhsIsbls ->
         (let _tlOsbls :: ([Node])
              _lhsObefs :: ([Node])
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: ([Maybe BoxTree])
              _hdOcss :: ([Selector])
              _hdOfathers :: ([Node])
              _tlOcss :: ([Selector])
              _tlOfathers :: ([Node])
              _hdInd :: Node 
              _hdIres :: (Maybe BoxTree)
              _hdIsel :: ([Maybe Selector])
              _tlIbefs :: ([Node])
              _tlIres :: ([Maybe BoxTree])
              _tlIsel :: ([Maybe Selector])
              _tlOsbls =
                  ({-# LINE 30 "./src/ag/NTree.ag" #-}
                   _hdInd : _lhsIsbls
                   {-# LINE 232 "./src/ag/NTree.hs" #-})
              _lhsObefs =
                  ({-# LINE 31 "./src/ag/NTree.ag" #-}
                   _lhsIsbls
                   {-# LINE 236 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 58 "./src/ag/NTree.ag" #-}
                   _hdIsel ++ _tlIsel
                   {-# LINE 240 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 75 "./src/ag/NTree.ag" #-}
                   _hdIres : _tlIres
                   {-# LINE 244 "./src/ag/NTree.hs" #-})
              _hdOcss =
                  ({-# LINE 62 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 248 "./src/ag/NTree.hs" #-})
              _hdOfathers =
                  ({-# LINE 19 "./src/ag/NTree.ag" #-}
                   _lhsIfathers
                   {-# LINE 252 "./src/ag/NTree.hs" #-})
              _tlOcss =
                  ({-# LINE 62 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 256 "./src/ag/NTree.hs" #-})
              _tlOfathers =
                  ({-# LINE 19 "./src/ag/NTree.ag" #-}
                   _lhsIfathers
                   {-# LINE 260 "./src/ag/NTree.hs" #-})
              ( _hdInd,_hdIres,_hdIsel) =
                  (hd_ _hdOcss _hdOfathers )
              ( _tlIbefs,_tlIres,_tlIsel) =
                  (tl_ _tlOcss _tlOfathers _tlOsbls )
          in  ( _lhsObefs,_lhsOres,_lhsOsel)))
sem_NTrees_Nil :: T_NTrees 
sem_NTrees_Nil  =
    (\ _lhsIcss
       _lhsIfathers
       _lhsIsbls ->
         (let _lhsObefs :: ([Node])
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: ([Maybe BoxTree])
              _lhsObefs =
                  ({-# LINE 32 "./src/ag/NTree.ag" #-}
                   _lhsIsbls
                   {-# LINE 277 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 59 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 281 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 76 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 285 "./src/ag/NTree.hs" #-})
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
              {-# LINE 313 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 317 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NStyle :: T_Node 
sem_Node_NStyle  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NStyle
              {-# LINE 325 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 329 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NTag :: String ->
                 T_Node 
sem_Node_NTag string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NTag string_
              {-# LINE 338 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 342 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NText :: String ->
                  T_Node 
sem_Node_NText string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NText string_
              {-# LINE 351 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 355 "./src/ag/NTree.hs" #-})
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
              {-# LINE 375 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 379 "./src/ag/NTree.hs" #-})
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
    (let _ntreeOfathers :: ([Node])
         _ntreeOcss :: ([Selector])
         _lhsOres :: BoxTree
         _ntreeInd :: Node 
         _ntreeIres :: (Maybe BoxTree)
         _ntreeIsel :: ([Maybe Selector])
         _ntreeOfathers =
             ({-# LINE 26 "./src/ag/NTree.ag" #-}
              []
              {-# LINE 403 "./src/ag/NTree.hs" #-})
         _ntreeOcss =
             ({-# LINE 64 "./src/ag/NTree.ag" #-}
              catMaybes _ntreeIsel
              {-# LINE 407 "./src/ag/NTree.hs" #-})
         _lhsOres =
             ({-# LINE 91 "./src/ag/NTree.ag" #-}
              fromJust _ntreeIres
              {-# LINE 411 "./src/ag/NTree.hs" #-})
         ( _ntreeInd,_ntreeIres,_ntreeIsel) =
             (ntree_ _ntreeOcss _ntreeOfathers )
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
              {-# LINE 435 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 439 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_SSelector_UnivSelector :: T_SSelector 
sem_SSelector_UnivSelector  =
    (let _lhsOnd :: SSelector 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              UnivSelector
              {-# LINE 447 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 451 "./src/ag/NTree.hs" #-})
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
              {-# LINE 477 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 481 "./src/ag/NTree.hs" #-})
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
              {-# LINE 495 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 499 "./src/ag/NTree.hs" #-})
         ( _sSelectorInd) =
             (sSelector_ )
     in  ( _lhsOnd))