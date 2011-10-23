-- |
-- Module      : Main
-- Copyright   : (c) 2011 Kevin Cantu
--
-- License     : BSD-style
-- Maintainer  : Kevin Cantu <me@kevincantu.org>
-- Stability   : experimental

module HilbertRTree (HilbertRTree(NewHRT), Tree(Info), insertHRT, searchHRT) where

import HilbertCoordinates
import Data.List

-- Hilbert H Tree
data Tree = Branch R H [Tree] -- contains branches and leaves
          | Leaf   R H [Tree] -- contains only infos
          | Info   R H String --
          deriving (Show, Ord, Eq)

data HilbertRTree = Root Tree
                  | NewHRT
                  deriving (Show, Ord, Eq)


leafIsNotFull :: [Tree] -> Bool
leafIsNotFull s = length s < 3

branchIsNotFull :: [Tree] -> Bool
branchIsNotFull s = length s < 3


trees2children :: [Tree] -- parents
                        -> [Tree] -- children
trees2children parents = unwrap' =<< parents
                    where
                       unwrap' (Branch _ _ children) = children
                       unwrap' (Leaf _ _ children) = children
                       unwrap' (Info _ _ _) = error "unwrapping an info makes no sense"


-- this seems ugly
collectR :: [Tree] -> MBR
collectR [] = error "the MBR of nothing is an error"
collectR (c:cs) =
   let
      f r (Info   r' _ _) = maxRect r r'
      f r (Leaf   r' _ _) = maxRect r r'
      f r (Branch r' _ _) = maxRect r r'

      r0' (Info   r _ _) = r
      r0' (Leaf   r _ _) = r
      r0' (Branch r _ _) = r

      maxRect (MBR (Pt x0 x1) (Pt y0 y1))
              (MBR (Pt x0' x1') (Pt y0' y1')) = MBR (Pt (min x0 x0') (min x1 x1'))
                                                    (Pt (max y0 y0') (max y1 y1'))
   in
      foldl f (r0' c) cs


collectH :: [Tree] -> LHV
collectH [] = error "the LHV of nothing is an error"
collectH (c:cs) =
   let
      f h' (Info   _ h _) = max h h'
      f h' (Leaf   _ h _) = max h h'
      f h' (Branch _ h _) = max h h'

      h0' (Info   _ h _) = h
      h0' (Leaf   _ h _) = h
      h0' (Branch _ h _) = h
   in
      foldl f (h0' c) cs


children2Tree :: [Tree] -> Tree
children2Tree children =
   let
      f cs@(Info   _ _ _ : _) = Leaf   r h cs
      f cs@(Leaf   _ _ _ : _) = Branch r h cs
      f cs@(Branch _ _ _ : _) = Branch r h cs
      f [] = error "empty tree makes no sense here..."

      r = collectR children
      h = collectH children
   in
      f children


shuffle :: [Tree] -- parents to shuffle
        -> [Tree]
shuffle trees =
   let
      children = trees2children trees
      trees' = unfoldr f children
             where
               len = length children `div` length trees
               f [] = Nothing
               f xs = Just (splitAt len xs)
   in
      map children2Tree trees'
      {- test:
      shuffle [Leaf r0 h0 [], 
               Leaf r0 h0 [Info (MBR (Pt 15 18) (Pt 37 29)) (LHV 67) "zzzzz",
                           Info r0 (LHV 90) "aaa"],
               Leaf r0 (LHV 9000) [Info r0 (LHV 700) "bbbbb"]]
      -}


-- big insert function for Trees, recursively
insertT :: Tree -- Info
        -> Tree -- Existing Leaf or Branch
        -> Maybe Tree

insertT _ (Info _ _ _) = error "sorry, inserting into an Info makes no sense"

insertT newi (Leaf _ _ infos) =
   if leafIsNotFull infos
   then Just $ children2Tree $ insert newi infos
   else Nothing

insertT newi (Branch _ _ trees) =
   let
      -- yes, this is a zipper... TODO
      (smallerTs, targetT, biggerTs) =
         let
            lessThanH (Info _ h' _) (Branch _ h'' _) = h' > h''
            lessThanH (Info _ h' _) (Leaf   _ h'' _) = h' > h''
            lessThanH _ _ = error "i am confused: what are you looking for?"

            (smallerH, biggerH) = span (lessThanH newi) trees
         in
            case (reverse smallerH, biggerH) of
               (sss, t:ts) -> (sss, t, ts)
               (s:ss, tts) -> (reverse ss, s, tts)
               (_, _)      -> error "well, this is an empty list..."

      siblings = smallerTs -- TODO: think about this

      siblingsNotFull s =
         let
            notFull (Branch _ _ xs) = branchIsNotFull xs
            notFull (Leaf _ _ xs)   = leafIsNotFull xs
            notFull (Info _ _ _)    = error "an Info can't be full or empty"
         in
            elem True . map notFull $ s

      addEmptySibling bs@(Branch _ _ _:_) = Branch r0 h0 [] : bs
      addEmptySibling ls@(Leaf   _ _ _:_) = Leaf   r0 h0 [] : ls
      addEmptySibling _ = error "sorry, new empty siblings of Info make no sense"

   in
      -- recursive call
      case trees of
         [] -> Nothing
               -- insertT (Info r0 h0 "wtf") (Branch r0 h0 [])

         _  ->
            case insertT newi targetT of
               Just t  -> Just $ children2Tree $ concat [smallerTs, [t], biggerTs]

                          -- insertT (Info r0 h0 "wtf") (Leaf r0 h0 [])
                          -- insertT (Info r0 h0 "wtf") (Branch r0 h0 [Leaf r0 h0 []])

               Nothing ->
                  case siblingsNotFull siblings of
                     True  -> insertT newi . children2Tree $ shuffle siblings
                     False ->
                        case branchIsNotFull trees of
                           False -> Nothing
                           True  -> -- question: how can I guarantee this will give
                                    --           my target tree free space for the recursive call?
                                    --

                                    insertT newi . children2Tree . shuffle $ addEmptySibling trees

                                    --  insertT (Info r0 h0 "wtf")
                                    --          (Branch r0 h0
                                    --             [Leaf r0 h0
                                    --                [Info r0 h0 "existing 0",
                                    --                 Info r0 h0 "existing 1",
                                    --                 Info r0 h0 "existing 2"]])



-- simple insert function for Hilbert R Trees
-- at the root level
insertHRT :: Tree           -- info to insert
            -> HilbertRTree -- existing tree
            -> HilbertRTree

insertHRT (Branch _ _ _) _ = error "sorry, not expecting you to insert a whole branch"
insertHRT (Leaf   _ _ _) _ = error "sorry, not expecting you to insert a whole leaf"

insertHRT newi@(Info r h _) NewHRT = insertHRT newi (Root emptyLeaf)
                      where emptyLeaf = Leaf r h [] -- insertion will overwrite r and h

insertHRT newi@(Info r h _) (Root tree) =
   case insertT newi tree of
      Just tree' -> Root tree'
      Nothing -> case insertT newi (Branch r h [tree]) of -- insertion will overwrite r and h
                   Just tree'' -> Root tree''
                   Nothing -> error "seriously, wtf"


-- query a Hilbert R Tree and return four values
searchHRT :: MBR          -- rectangle query
          -> HilbertRTree -- existing tree
          -> [Tree]       -- first infos overlapped by this box

searchHRT r (Root tree) = searchT r tree -- possibly limit to (take 4)
searchHRT _ NewHRT = []

-- point intersecting a rectangle
intersectPt :: Pt -> MBR -> Bool
intersectPt (Pt a b) (MBR (Pt x y) (Pt x' y')) = 
   x <= a || a <= x' && 
   y <= b || b <= y' 

-- rectangle overlapping
intersectRect :: MBR -> MBR -> Bool
intersectRect (MBR (Pt x y) (Pt x' y')) rect =
   intersectPt (Pt x  y ) rect ||
   intersectPt (Pt x' y') rect ||
   intersectPt (Pt x  y') rect ||
   intersectPt (Pt x' y ) rect

getR :: Tree -> MBR
getR (Info   x _ _) = x
getR (Leaf   x _ _) = x
getR (Branch x _ _) = x

searchT :: MBR -> Tree -> [Tree]
searchT r (Branch _ _ trees) =
   let
      f :: [Tree] -> [Tree]
      f = filter (intersectRect r . getR)
   in
      (searchT r) =<< f trees

searchT r (Leaf _ _ infos) = filter (intersectRect r . getR) infos

searchT _ (Info _ _ _) = error "actually, we're not recursing that far, but maybe later..."
   
              



