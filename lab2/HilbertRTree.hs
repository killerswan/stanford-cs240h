-- Copyright (c) 2011 Kevin Cantu

module HilbertRTree ( HilbertRTree(NewHRT)
                    , Tree(Info)
                    , insertHRT
                    , searchHRT
                    , intersectPt
                    , mutualIntersectRect
                    ) where

import HilbertCoordinates
import Data.List

-- Hilbert H Tree
data Tree = Branch Rect LHV [Tree] -- contains branches and leaves
          | Leaf   Rect LHV [Tree] -- contains only infos
          | Info   Rect LHV String --
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
collectR :: [Tree] -> Rect
collectR [] = error "the MBR of nothing is an error"
collectR (c:cs) =
   let
      f r (Info   r' _ _) = maxRect r r'
      f r (Leaf   r' _ _) = maxRect r r'
      f r (Branch r' _ _) = maxRect r r'

      r0' (Info   r _ _) = r
      r0' (Leaf   r _ _) = r
      r0' (Branch r _ _) = r

      maxRect (Rect (Pt x0 x1) (Pt y0 y1))
              (Rect (Pt x0' x1') (Pt y0' y1')) = Rect (Pt (min x0 x0') (min x1 x1'))
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

      -- It is necessary that the number of new free spots created
      -- when a new item is added guarantees that the next insert 
      -- will succeed.
      -- One way is to make sure that the number of spots per subtree
      -- equals the number of subtrees minus one.
      -- So here I am going to make all peers into siblings,
      -- rather than split along the lines of smaller/greater and target.
      (siblings, nonSiblings) = (trees, [])

      siblingsNotFull s =
         let
            notFull (Branch _ _ xs) = branchIsNotFull xs
            notFull (Leaf _ _ xs)   = leafIsNotFull xs
            notFull (Info _ _ _)    = error "an Info can't be full or empty"
         in
            elem True . map notFull $ s

      ee = error "this shouldn't be reached #A"
      addEmptySibling bs@(Branch _ _ _:_) = Branch ee ee [] : bs
      addEmptySibling ls@(Leaf   _ _ _:_) = Leaf   ee ee [] : ls
      addEmptySibling _ = error "sorry, new empty siblings of Info make no sense"

   in
      -- recursive call
      case trees of
         [] -> Nothing
         _  ->
            case insertT newi targetT of
               Just t  -> Just $ children2Tree $ concat [smallerTs, [t], biggerTs]
               Nothing ->
                  case siblingsNotFull siblings of
                     True  -> insertT newi . children2Tree $ concat [shuffle siblings, nonSiblings]
                     False ->
                        case branchIsNotFull trees of
                           False -> Nothing
                           True  -> -- question: how can I guarantee this will give
                                    --           my target tree free space for the recursive call?
                                    --
                                    insertT newi . children2Tree . concat $ [shuffle (addEmptySibling siblings), nonSiblings]


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
searchHRT :: Rect         -- rectangle query
          -> HilbertRTree -- existing tree
          -> [Tree]       -- first infos overlapped by this box

searchHRT _ NewHRT      = []
searchHRT r (Root tree) = searchT r tree -- possibly limit to (take 4)


-- point intersecting a rectangle
intersectPt :: Pt -> Rect -> Bool
intersectPt (Pt a b) (Rect (Pt x y) (Pt x' y')) = 
   x <= a && a <= x' && 
   y <= b && b <= y'


-- rectangle overlapping
mutualIntersectRect :: Rect -> Rect -> Bool
mutualIntersectRect a b =
   let
      intersectRect (Rect (Pt x y) (Pt x' y')) rect =
         intersectPt (Pt x  y ) rect ||
         intersectPt (Pt x' y') rect ||
         intersectPt (Pt x  y') rect ||
         intersectPt (Pt x' y ) rect
   in
      intersectRect a b || intersectRect b a


getR :: Tree -> Rect
getR (Info   x _ _) = x
getR (Leaf   x _ _) = x
getR (Branch x _ _) = x


searchT :: Rect -> Tree -> [Tree]
searchT _ (Info _ _ _)       = error "searchT doesn't work on Info yet..."
searchT r (Leaf _ _ infos)   = filter (mutualIntersectRect r . getR) infos
searchT r (Branch _ _ trees) =
   let
      f :: [Tree] -> [Tree]
      f = filter (mutualIntersectRect r . getR)
   in
      searchT r =<< f trees

   
