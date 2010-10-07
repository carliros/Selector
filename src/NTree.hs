

-- UUAGC 0.9.29 (./src/ag/NTree.ag)
module NTree where
{-# LINE 5 "./src/ag/NTree.ag" #-}

--import DataCss
import Data.List
import Data.Maybe
import FSTree
{-# LINE 12 "./src/ag/NTree.hs" #-}
{-# LINE 40 "./src/ag/NTree.ag" #-}

defaultStyle :: [Selector]
defaultStyle = [ SimpSelector (TypeSelector "big")
               , CombSelector (TypeSelector "body") ">" (CombSelector (TypeSelector "p") " " (SimpSelector (TypeSelector "big")))
               , SimpSelector (TypeSelector "small")
               , SimpSelector UnivSelector
               ]
{-# LINE 21 "./src/ag/NTree.hs" #-}

{-# LINE 95 "./src/ag/NTree.ag" #-}

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

--applySelector :: Node -> [Node] -> [Node] -> [Node] -> Int -> [TSelector] -> Bool
applySelector    _       _         _         _         _      []
    = True
applySelector    nd      fathers   siblings  before    count  (sel:nextSel)
    = case sel of
        TSimplSelector s -> applySimplSelector nd fathers siblings before count s nextSel
        TDescdSelector s -> applyDescdSelector nd fathers siblings before count s nextSel
        TChildSelector s -> applyChildSelector nd fathers siblings before count s nextSel
        TSiblnSelector s -> applySiblnSelector nd fathers siblings before count s nextSel

applySimplSelector nd fathers siblings before count s nextSel
    = if testSimpleSelector s nd
      then applySelector nd fathers siblings before (count+1) nextSel
      else False

applyDescdSelector _  []     _        _      _     _ _
    = False
applyDescdSelector nd (f:fs) siblings before count s nextSel
    = if testSimpleSelector s (fst f)
      then if applySelector nd fs siblings (f:before) (count+1) nextSel
           then True
           else applyDescdSelector nd fs siblings before count s nextSel
      else applyDescdSelector nd fs siblings before count s nextSel

applyChildSelector _  []     _        _      _     _ _
    = False
applyChildSelector nd (f:fs) siblings before count s nextSel
    = if testSimpleSelector s (fst f)
      then applySelector nd fs siblings (f:before) (count+1) nextSel
      else False

applySiblnSelector nd fathers siblings before count s nextSel
    = let brothers = if count<=1 then siblings else snd $ head before
      in let ntest = case s of
                        TypeSelector nm -> let (bool,ts) = getNextValidTag brothers
                                           in if bool
                                              then ((NTag nm) == (head ts), tail ts)
                                              else (False, [])  -- the empty list it's not important because it won't use any more
                        UnivSelector    -> let (bool,ts) = getNextValidTag brothers
                                           in if bool
                                              then (True, tail ts)
                                              else (False, [])  -- the empty list it's not important because it won't use any more
         in if fst ntest
            then if count<=1
                 then applySelector nd fathers (snd ntest) before count nextSel
                 else applySelector nd fathers siblings (let (f,_)=head before in (f,snd ntest):(tail before)) count nextSel
            else False

getNextValidTag []             = (False, [])
getNextValidTag l@((NTag _):_) = (True, l)
getNextValidTag (_:xs)         = getNextValidTag xs

testSimpleSelector s nd = case s of
                            TypeSelector nm -> nd == (NTag nm)
                            UnivSelector    -> True
{-# LINE 93 "./src/ag/NTree.hs" #-}

{-# LINE 30 "./src/ag//DataTree.ag" #-}

instance Eq Node where
    NTag n1 == NTag n2 = n1 == n2
    _      == _      = False
{-# LINE 100 "./src/ag/NTree.hs" #-}

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
{-# LINE 121 "./src/ag/NTree.hs" #-}
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
                ([(Node, [Node])]) ->
                ([Node]) ->
                ( Node ,(Maybe BoxTree),([Maybe Selector]))
sem_NTree_NTree :: T_Node  ->
                   T_NTrees  ->
                   T_NTree 
sem_NTree_NTree node_ ntrees_  =
    (\ _lhsIcss
       _lhsIfathers
       _lhsIsiblings ->
         (let _lhsOnd :: Node 
              _ntreesOfathers :: ([(Node, [Node])])
              _ntreesOsiblings :: ([Node])
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: (Maybe BoxTree)
              _ntreesOcss :: ([Selector])
              _nodeInd :: Node 
              _ntreesIres :: ([Maybe BoxTree])
              _ntreesIsel :: ([Maybe Selector])
              _lhsOnd =
                  ({-# LINE 17 "./src/ag/NTree.ag" #-}
                   _nodeInd
                   {-# LINE 154 "./src/ag/NTree.hs" #-})
              _ntreesOfathers =
                  ({-# LINE 22 "./src/ag/NTree.ag" #-}
                   _nfs     : _lhsIfathers
                   {-# LINE 158 "./src/ag/NTree.hs" #-})
              _fathers =
                  ({-# LINE 23 "./src/ag/NTree.ag" #-}
                   _lhsIfathers
                   {-# LINE 162 "./src/ag/NTree.hs" #-})
              _nfs =
                  ({-# LINE 24 "./src/ag/NTree.ag" #-}
                   (_nodeInd, _siblings    )
                   {-# LINE 166 "./src/ag/NTree.hs" #-})
              _ntreesOsiblings =
                  ({-# LINE 36 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 170 "./src/ag/NTree.hs" #-})
              _siblings =
                  ({-# LINE 37 "./src/ag/NTree.ag" #-}
                   _lhsIsiblings
                   {-# LINE 174 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 52 "./src/ag/NTree.ag" #-}
                   let nsel = case _nodeInd of
                                 NTag _         -> Nothing
                                 NText _        -> Nothing
                                 NStyle         -> Nothing
                                 NRuleCss (s,p) -> Just s
                   in nsel : _ntreesIsel
                   {-# LINE 183 "./src/ag/NTree.hs" #-})
              _rlist =
                  ({-# LINE 71 "./src/ag/NTree.ag" #-}
                   map (applySelector _nodeInd _fathers     (reverse _siblings    ) [] 0) (map (\sel -> reverse (transformSelector sel)) _lhsIcss)
                   {-# LINE 187 "./src/ag/NTree.hs" #-})
              _sel =
                  ({-# LINE 72 "./src/ag/NTree.ag" #-}
                   or _rlist
                   {-# LINE 191 "./src/ag/NTree.hs" #-})
              _num =
                  ({-# LINE 73 "./src/ag/NTree.ag" #-}
                   if _sel     then fromMaybe (-1) (elemIndex True _rlist    ) + 1 else 0
                   {-# LINE 195 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 82 "./src/ag/NTree.ag" #-}
                   let nd = case _nodeInd of
                             NTag  str  -> Just $ BoxTag  _sel     _num     str
                             NText str  -> Just $ BoxText _sel     _num     str
                             NStyle     -> Nothing
                             NRuleCss _ -> Nothing
                   in if isNothing nd
                      then Nothing
                      else Just $ BoxTree (fromJust nd) (catMaybes _ntreesIres)
                   {-# LINE 206 "./src/ag/NTree.hs" #-})
              _ntreesOcss =
                  ({-# LINE 64 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 210 "./src/ag/NTree.hs" #-})
              ( _nodeInd) =
                  (node_ )
              ( _ntreesIres,_ntreesIsel) =
                  (ntrees_ _ntreesOcss _ntreesOfathers _ntreesOsiblings )
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
                 ([(Node, [Node])]) ->
                 ([Node]) ->
                 ( ([Maybe BoxTree]),([Maybe Selector]))
sem_NTrees_Cons :: T_NTree  ->
                   T_NTrees  ->
                   T_NTrees 
sem_NTrees_Cons hd_ tl_  =
    (\ _lhsIcss
       _lhsIfathers
       _lhsIsiblings ->
         (let _tlOsiblings :: ([Node])
              _hdOsiblings :: ([Node])
              _lhsOsel :: ([Maybe Selector])
              _lhsOres :: ([Maybe BoxTree])
              _hdOcss :: ([Selector])
              _hdOfathers :: ([(Node, [Node])])
              _tlOcss :: ([Selector])
              _tlOfathers :: ([(Node, [Node])])
              _hdInd :: Node 
              _hdIres :: (Maybe BoxTree)
              _hdIsel :: ([Maybe Selector])
              _tlIres :: ([Maybe BoxTree])
              _tlIsel :: ([Maybe Selector])
              _tlOsiblings =
                  ({-# LINE 32 "./src/ag/NTree.ag" #-}
                   _hdInd : _lhsIsiblings
                   {-# LINE 251 "./src/ag/NTree.hs" #-})
              _hdOsiblings =
                  ({-# LINE 33 "./src/ag/NTree.ag" #-}
                   _lhsIsiblings
                   {-# LINE 255 "./src/ag/NTree.hs" #-})
              _lhsOsel =
                  ({-# LINE 60 "./src/ag/NTree.ag" #-}
                   _hdIsel ++ _tlIsel
                   {-# LINE 259 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 77 "./src/ag/NTree.ag" #-}
                   _hdIres : _tlIres
                   {-# LINE 263 "./src/ag/NTree.hs" #-})
              _hdOcss =
                  ({-# LINE 64 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 267 "./src/ag/NTree.hs" #-})
              _hdOfathers =
                  ({-# LINE 19 "./src/ag/NTree.ag" #-}
                   _lhsIfathers
                   {-# LINE 271 "./src/ag/NTree.hs" #-})
              _tlOcss =
                  ({-# LINE 64 "./src/ag/NTree.ag" #-}
                   _lhsIcss
                   {-# LINE 275 "./src/ag/NTree.hs" #-})
              _tlOfathers =
                  ({-# LINE 19 "./src/ag/NTree.ag" #-}
                   _lhsIfathers
                   {-# LINE 279 "./src/ag/NTree.hs" #-})
              ( _hdInd,_hdIres,_hdIsel) =
                  (hd_ _hdOcss _hdOfathers _hdOsiblings )
              ( _tlIres,_tlIsel) =
                  (tl_ _tlOcss _tlOfathers _tlOsiblings )
          in  ( _lhsOres,_lhsOsel)))
sem_NTrees_Nil :: T_NTrees 
sem_NTrees_Nil  =
    (\ _lhsIcss
       _lhsIfathers
       _lhsIsiblings ->
         (let _lhsOsel :: ([Maybe Selector])
              _lhsOres :: ([Maybe BoxTree])
              _lhsOsel =
                  ({-# LINE 61 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 295 "./src/ag/NTree.hs" #-})
              _lhsOres =
                  ({-# LINE 78 "./src/ag/NTree.ag" #-}
                   []
                   {-# LINE 299 "./src/ag/NTree.hs" #-})
          in  ( _lhsOres,_lhsOsel)))
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
              {-# LINE 327 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 331 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NStyle :: T_Node 
sem_Node_NStyle  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NStyle
              {-# LINE 339 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 343 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NTag :: String ->
                 T_Node 
sem_Node_NTag string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NTag string_
              {-# LINE 352 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 356 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_Node_NText :: String ->
                  T_Node 
sem_Node_NText string_  =
    (let _lhsOnd :: Node 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              NText string_
              {-# LINE 365 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 369 "./src/ag/NTree.hs" #-})
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
              {-# LINE 389 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 393 "./src/ag/NTree.hs" #-})
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
    (let _ntreeOfathers :: ([(Node, [Node])])
         _ntreeOsiblings :: ([Node])
         _ntreeOcss :: ([Selector])
         _lhsOres :: BoxTree
         _ntreeInd :: Node 
         _ntreeIres :: (Maybe BoxTree)
         _ntreeIsel :: ([Maybe Selector])
         _ntreeOfathers =
             ({-# LINE 27 "./src/ag/NTree.ag" #-}
              []
              {-# LINE 418 "./src/ag/NTree.hs" #-})
         _ntreeOsiblings =
             ({-# LINE 28 "./src/ag/NTree.ag" #-}
              []
              {-# LINE 422 "./src/ag/NTree.hs" #-})
         _ntreeOcss =
             ({-# LINE 66 "./src/ag/NTree.ag" #-}
              catMaybes _ntreeIsel
              {-# LINE 426 "./src/ag/NTree.hs" #-})
         _lhsOres =
             ({-# LINE 93 "./src/ag/NTree.ag" #-}
              fromJust _ntreeIres
              {-# LINE 430 "./src/ag/NTree.hs" #-})
         ( _ntreeInd,_ntreeIres,_ntreeIsel) =
             (ntree_ _ntreeOcss _ntreeOfathers _ntreeOsiblings )
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
              {-# LINE 454 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 458 "./src/ag/NTree.hs" #-})
     in  ( _lhsOnd))
sem_SSelector_UnivSelector :: T_SSelector 
sem_SSelector_UnivSelector  =
    (let _lhsOnd :: SSelector 
         _nd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              UnivSelector
              {-# LINE 466 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 470 "./src/ag/NTree.hs" #-})
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
              {-# LINE 496 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 500 "./src/ag/NTree.hs" #-})
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
              {-# LINE 514 "./src/ag/NTree.hs" #-})
         _lhsOnd =
             ({-# LINE 14 "./src/ag/NTree.ag" #-}
              _nd
              {-# LINE 518 "./src/ag/NTree.hs" #-})
         ( _sSelectorInd) =
             (sSelector_ )
     in  ( _lhsOnd))