{-# LANGUAGE BangPatterns, KindSignatures, DataKinds, GADTs, FlexibleInstances #-}

module Range where

import Linear

data Nat = Z | S Nat

data Vec (n :: Nat) (a :: *) where 
    Nil  :: Vec Z a 
    (:<) :: Vec n a -> a -> Vec (S n) a 
infixl 5 :<

class FoldrDimension n where 
    foldrRangeN :: (Vec n Int -> t -> t) -> t -> Vec n Int -> Vec n Int -> t 

instance FoldrDimension Z where
    {-# INLINE foldrRangeN #-}
    foldrRangeN f x Nil Nil = f Nil x 

instance FoldrDimension n => FoldrDimension (S n) where 
    {-# INLINE foldrRangeN #-}
    foldrRangeN cons nil (ax :< ay) (bx :< by) = go (by-1) nil where 
        go by !r | by < ay   = r
                 | otherwise = go (by-1) (foldrRangeN (\ ax -> cons (ax :< by)) r ax bx)

class FoldrRange a where
    foldrRange :: (a -> t -> t) -> t -> a -> a -> t

instance FoldrRange Int where
    {-# INLINE foldrRange #-}
    foldrRange cons nil a0 b0 = foldrRangeN cons' nil from to where
        cons' (Nil :< x)      = cons x
        from                  = (Nil :< a0)
        to                    = (Nil :< b0)

instance FoldrRange (Int,Int) where
    {-# INLINE foldrRange #-}
    foldrRange cons nil (a0,a1) (b0,b1) = foldrRangeN cons' nil from to where
        cons' (Nil :< x0 :< x1)         = cons (x0,x1)
        from                            = (Nil :< a0 :< a1)
        to                              = (Nil :< b0 :< b1)

instance FoldrRange (Int,Int,Int) where
    {-# INLINE foldrRange #-}
    foldrRange cons nil (a0,a1,a2) (b0,b1,b2) = foldrRangeN cons' nil from to where
        cons' (Nil :< x0 :< x1 :< x2)         = cons (x0,x1,x2)
        from                                  = (Nil :< a0 :< a1 :< a2)
        to                                    = (Nil :< b0 :< b1 :< b2)

instance FoldrRange (Int,Int,Int,Int) where
    {-# INLINE foldrRange #-}
    foldrRange cons nil (a0,a1,a2,a3) (b0,b1,b2,b3) = foldrRangeN cons' nil from to where
        cons' (Nil :< x0 :< x1 :< x2 :< x3)         = cons (x0,x1,x2,x3)
        from                                        = (Nil :< a0 :< a1 :< a2 :< a3)
        to                                          = (Nil :< b0 :< b1 :< b2 :< b3)

instance FoldrRange (V2 Int) where
    {-# INLINE foldrRange #-}
    foldrRange cons nil (V2 a0 a1) (V2 b0 b1) = foldrRangeN cons' nil from to where
        cons' (Nil :< x0 :< x1)               = cons (V2 x0 x1)
        from                                  = (Nil :< a0 :< a1)
        to                                    = (Nil :< b0 :< b1)

instance FoldrRange (V3 Int) where
    {-# INLINE foldrRange #-}
    foldrRange cons nil (V3 a0 a1 a2) (V3 b0 b1 b2) = foldrRangeN cons' nil from to where
        cons' (Nil :< x0 :< x1 :< x2)               = cons (V3 x0 x1 x2)
        from                                        = (Nil :< a0 :< a1 :< a2)
        to                                          = (Nil :< b0 :< b1 :< b2)

mapRange :: FoldrRange a => (a -> b) -> a -> a -> [b]
mapRange f = foldrRange ((:).f) []

forRange :: (Monad m, FoldrRange a) => (a -> m b) -> a -> a -> m ()
forRange f = foldrRange ((>>).f) (return ())

for :: (Monad m, FoldrRange a) => a -> a -> (a -> m b) -> m ()
for a b f = forRange f a b
