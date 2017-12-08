module Data.List.Zipper
  ( Zipper(..)
  , beginp
  , endp
  , left
  , right
  , cursor
  , replace
  , fromList
  ) where

data Zipper a =
  Zipper [a]
         [a]

beginp :: Zipper a -> Bool
beginp (Zipper [] _) = True
beginp _ = False

endp :: Zipper a -> Bool
endp (Zipper _ []) = True
endp _ = False

left :: Zipper a -> Zipper a
left (Zipper [] rs) = Zipper [] rs
left (Zipper (l:ls) rs) = Zipper ls (l : rs)

right :: Zipper a -> Zipper a
right (Zipper ls []) = Zipper ls []
right (Zipper ls (r:rs)) = Zipper (r : ls) rs

cursor :: Zipper a -> Maybe a
cursor (Zipper _ []) = Nothing
cursor (Zipper _ (r:rs)) = Just r

replace :: a -> Zipper a -> Zipper a
replace a (Zipper ls []) = Zipper ls [a]
replace a (Zipper ls (r:rs)) = Zipper ls (a : rs)

fromList :: [a] -> Zipper a
fromList = Zipper []
