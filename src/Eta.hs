{-# LANGUAGE PolyKinds
           , RankNTypes
           , BangPatterns
  #-}
module Eta (
(.), ($), (&), ($!), curry, uncurry, flip, on, id,
) where


import Prelude (id, seq)
import GHC.Exts (TYPE)


infixr 9 .
(.) :: forall r a b (c :: TYPE r). (b -> c) -> (a -> b) -> a -> c
{-^ @(g . f) x@ is equivalent to @g (f x)@.
@(.) 'undefined' = 'undefined'@
-}
(.) g = g `seq` \ f x -> g (f x)
{-# INLINABLE (.) #-}

infixr 0 $
($) :: forall ri ro (a :: TYPE ri) (b :: TYPE ro). (a -> b) -> a -> b
($) f = f
{-# INLINABLE ($) #-}

infixr 0 $!
($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
{-^ @($!) f@ makes @f@ strict in its first argument.
@($!) 'undefined' = 'undefined'@
@('undefined' $!) `seq` () = 'undefined' -- with PostfixOperators@
@('undefined' $!) `seq` () = () -- without PostfixOperators@
-}
-- ($!) f = f `seq` \ x -> x `seq` f x
($!) f = f `seq` \ x -> let !x' = x in f x'
{-# INLINABLE ($!) #-}

infixl 1 &
(&) :: forall r a (b :: TYPE r). a -> (a -> b) -> b
{-^ @x & f = f x@
@y & x & g = g (x y)@ -- not @g x y@!

>>> (& 'undefined') `seq` ()
()
-}
x & f = f x
{-# INLINABLE (&) #-}

-- infixl 1 !&
-- (!&) :: a -> (a -> b) -> b
-- {-^ Evaluate a value, then apply a function to it.
-- @y !& x !& g = g $! x $! y@ -- not @y !& (x !& g)@!
-- @(!& 'undefined') `seq` () = ()@
-- -}
-- x !& f = x `seq` f x
-- {-# INLINABLE (!&) #-}

curry :: forall r a b (c :: TYPE r). ((a, b) -> c) -> a -> b -> c
{-^ @curry f x y = f (x, y)@
@curry 'undefined' = 'undefined'@
-}
curry f = f `seq` \ x y -> f (x, y)
{-# INLINABLE curry #-}

uncurry :: forall r a b (c :: TYPE r). (a -> b -> c) -> (a, b) -> c
{-^ @uncurry f (x, y) = f x y@
@uncurry 'undefined' = 'undefined'@
Currently, @uncurry f 'undefined' = f 'undefined' 'undefined'@. This matches Data.Tuple.uncurry. Is this the "correct" behavior?
-}
uncurry f = f `seq` \ ~(x, y) -> f x y
{-# INLINABLE uncurry #-}

flip :: forall r a b (c :: TYPE r). (a -> b -> c) -> b -> a -> c
{-^ @flip f y x = f x y@
-}
flip f = f `seq` \ y x -> f x y
{-# INLINABLE flip #-}

infixl 0 `on`
on :: forall r a b (c :: TYPE r). (b -> b -> c) -> (a -> b) -> a -> a -> c
{-^ @on f g x y = f (g x) (g y)@
@g `on` 'id'  =  g@
@on 'undefined' = 'undefined'@
@('undefined' `on`) `seq` () = 'undefined'@ -- with PostfixOperators
@('undefined' `on`) `seq` () = ()@ -- without PostfixOperators
-}
on g = g `seq` \ f x y -> g (f x) (f y)
{-# INLINABLE on #-}
