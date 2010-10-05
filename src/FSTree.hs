

-- UUAGC 0.9.29 (./src/ag/FSTree.ag)
module FSTree where

xInit = 10
yInit = 10
xSep = 40
ySep = 80
xBox = 80
yBox = 50

-- formatting datatypes
data OBox = Block Int Int Bool
          | Name String Int Int
          | Line (Int,Int) (Int, Int)
        deriving Show

-- BoxNode -----------------------------------------------------
data BoxNode  = BoxTag (Bool) (Int) (String) 
              | BoxText (Bool) (Int) (String) 
              deriving ( Show)
-- cata
sem_BoxNode :: BoxNode  ->
               T_BoxNode 
sem_BoxNode (BoxTag _bool _int _string )  =
    (sem_BoxNode_BoxTag _bool _int _string )
sem_BoxNode (BoxText _bool _int _string )  =
    (sem_BoxNode_BoxText _bool _int _string )
-- semantic domain
type T_BoxNode  = ( Bool,String)
sem_BoxNode_BoxTag :: Bool ->
                      Int ->
                      String ->
                      T_BoxNode 
sem_BoxNode_BoxTag bool_ int_ string_  =
    (let _lhsOnm :: String
         _lhsObl :: Bool
         _lhsOnm =
             if bool_ then string_ ++ "\n" ++ show int_ else string_
         _lhsObl =
             bool_
     in  ( _lhsObl,_lhsOnm))
sem_BoxNode_BoxText :: Bool ->
                       Int ->
                       String ->
                       T_BoxNode 
sem_BoxNode_BoxText bool_ int_ string_  =
    (let _lhsOnm :: String
         _lhsObl :: Bool
         _lhsOnm =
             if bool_ then "text"  ++ "\n" ++ show int_ else "text"
         _lhsObl =
             bool_
     in  ( _lhsObl,_lhsOnm))
-- BoxRoot -----------------------------------------------------
data BoxRoot  = BoxRoot (BoxTree ) 
              deriving ( Show)
-- cata
sem_BoxRoot :: BoxRoot  ->
               T_BoxRoot 
sem_BoxRoot (BoxRoot _boxTree )  =
    (sem_BoxRoot_BoxRoot (sem_BoxTree _boxTree ) )
-- semantic domain
type T_BoxRoot  = ( ([OBox]))
sem_BoxRoot_BoxRoot :: T_BoxTree  ->
                       T_BoxRoot 
sem_BoxRoot_BoxRoot boxTree_  =
    (let _boxTreeOyPos :: Int
         _boxTreeOxPos :: Int
         _lhsOout :: ([OBox])
         _boxTreeIlen :: Int
         _boxTreeIout :: ([OBox])
         _boxTreeIvec :: ((Int,Int))
         _boxTreeIxPos :: Int
         _boxTreeOyPos =
             yInit
         _boxTreeOxPos =
             xInit
         _lhsOout =
             _boxTreeIout
         ( _boxTreeIlen,_boxTreeIout,_boxTreeIvec,_boxTreeIxPos) =
             (boxTree_ _boxTreeOxPos _boxTreeOyPos )
     in  ( _lhsOout))
-- BoxTree -----------------------------------------------------
data BoxTree  = BoxTree (BoxNode ) (Boxes ) 
              deriving ( Show)
-- cata
sem_BoxTree :: BoxTree  ->
               T_BoxTree 
sem_BoxTree (BoxTree _boxNode _boxes )  =
    (sem_BoxTree_BoxTree (sem_BoxNode _boxNode ) (sem_Boxes _boxes ) )
-- semantic domain
type T_BoxTree  = Int ->
                  Int ->
                  ( Int,([OBox]),((Int,Int)),Int)
sem_BoxTree_BoxTree :: T_BoxNode  ->
                       T_Boxes  ->
                       T_BoxTree 
sem_BoxTree_BoxTree boxNode_ boxes_  =
    (\ _lhsIxPos
       _lhsIyPos ->
         (let _boxesOyPos :: Int
              _lhsOlen :: Int
              _boxesOxPos :: Int
              _lhsOxPos :: Int
              _lhsOvec :: ((Int,Int))
              _lhsOout :: ([OBox])
              _boxNodeIbl :: Bool
              _boxNodeInm :: String
              _boxesIlen :: Int
              _boxesIout :: ([OBox])
              _boxesIvec2s :: ([(Int,Int)])
              _boxesIxPos :: Int
              _yPos =
                  _lhsIyPos
              _boxesOyPos =
                  _lhsIyPos + ySep
              _len =
                  if _boxesIlen == 0
                  then xBox + xSep
                  else _boxesIlen
              _lhsOlen =
                  _len
              _boxesOxPos =
                  _lhsIxPos
              _lhsOxPos =
                  _boxesIxPos
              _xPos =
                  _lhsIxPos + (_len     `div` 2) + (xSep `div` 2)
              _lhsOvec =
                  (_xPos    ,_yPos    )
              _lhsOout =
                  let vec1   = (_xPos     + (xBox `div` 2), _yPos     + yBox)
                      cmdVec = map (\vec2 -> Line vec1 vec2) _boxesIvec2s
                  in ( (Block _xPos     _yPos     _boxNodeIbl) :
                       (Name _boxNodeInm (_xPos    +10) (_yPos    +10)) :
                       _boxesIout ) ++ cmdVec
              ( _boxNodeIbl,_boxNodeInm) =
                  (boxNode_ )
              ( _boxesIlen,_boxesIout,_boxesIvec2s,_boxesIxPos) =
                  (boxes_ _boxesOxPos _boxesOyPos )
          in  ( _lhsOlen,_lhsOout,_lhsOvec,_lhsOxPos)))
-- Boxes -------------------------------------------------------
type Boxes  = [BoxTree ]
-- cata
sem_Boxes :: Boxes  ->
             T_Boxes 
sem_Boxes list  =
    (Prelude.foldr sem_Boxes_Cons sem_Boxes_Nil (Prelude.map sem_BoxTree list) )
-- semantic domain
type T_Boxes  = Int ->
                Int ->
                ( Int,([OBox]),([(Int,Int)]),Int)
sem_Boxes_Cons :: T_BoxTree  ->
                  T_Boxes  ->
                  T_Boxes 
sem_Boxes_Cons hd_ tl_  =
    (\ _lhsIxPos
       _lhsIyPos ->
         (let _lhsOlen :: Int
              _hdOxPos :: Int
              _tlOxPos :: Int
              _lhsOxPos :: Int
              _lhsOvec2s :: ([(Int,Int)])
              _lhsOout :: ([OBox])
              _hdOyPos :: Int
              _tlOyPos :: Int
              _hdIlen :: Int
              _hdIout :: ([OBox])
              _hdIvec :: ((Int,Int))
              _hdIxPos :: Int
              _tlIlen :: Int
              _tlIout :: ([OBox])
              _tlIvec2s :: ([(Int,Int)])
              _tlIxPos :: Int
              _lhsOlen =
                  _hdIlen + _tlIlen
              _hdOxPos =
                  _lhsIxPos
              _tlOxPos =
                  _lhsIxPos + _hdIlen
              _lhsOxPos =
                  _tlIxPos
              _lhsOvec2s =
                  ((fst _hdIvec) + (xBox `div` 2), snd _hdIvec) : _tlIvec2s
              _lhsOout =
                  _hdIout ++ _tlIout
              _hdOyPos =
                  _lhsIyPos
              _tlOyPos =
                  _lhsIyPos
              ( _hdIlen,_hdIout,_hdIvec,_hdIxPos) =
                  (hd_ _hdOxPos _hdOyPos )
              ( _tlIlen,_tlIout,_tlIvec2s,_tlIxPos) =
                  (tl_ _tlOxPos _tlOyPos )
          in  ( _lhsOlen,_lhsOout,_lhsOvec2s,_lhsOxPos)))
sem_Boxes_Nil :: T_Boxes 
sem_Boxes_Nil  =
    (\ _lhsIxPos
       _lhsIyPos ->
         (let _lhsOlen :: Int
              _lhsOxPos :: Int
              _lhsOvec2s :: ([(Int,Int)])
              _lhsOout :: ([OBox])
              _lhsOlen =
                  0
              _lhsOxPos =
                  _lhsIxPos
              _lhsOvec2s =
                  []
              _lhsOout =
                  []
          in  ( _lhsOlen,_lhsOout,_lhsOvec2s,_lhsOxPos)))