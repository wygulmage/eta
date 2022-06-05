{-# LANGUAGE PolyKinds
           , ExplicitForAll
           , BangPatterns
  #-}
{-# OPTIONS_GHC -fno-do-lambda-eta-expansion #-}
-- Without -fno-do-lambda-eta-expansion, (.) is redefined to \ !g f x -> g (f x), which is not eta equivalent!

module Eta (
(.), ($), (&), ($!), curry, uncurry, flip, on, id,
) where


import Prelude (id, seq)
import GHC.Exts (TYPE)


infixr 9 .
(.) :: forall r a b (c :: TYPE r). (b -> c) -> (a -> b) -> a -> c
{-^ @(g . f) x@ is equivalent to @g (f x)@.
@(.) 'Prelude.undefined' = 'Prelude.undefined'@
@f . 'id' = f@

I want four things to hold: @(g . f) x = g (f x)@, @f . id  =  f@, @id . f  =  f@, @h . (f . g)  =  (h . g) . f@.

Different definitions:

1. @(.) g f x = g (f x)@ ('Prelude..')
This fails both 'id' identities:
@(Prelude.undefined . id) `seq` () = () -- should be 'Prelude.undefined'@
@(id . Prelude.undefined) `seq` () = () -- should be 'Prelude.undefined'@

2. @(.) g = g `seq` \ f x -> g (f x)@
This fails only the last 'id' identity:
@(id . undefined) `seq` () = () -- should be 'Prelude.undefined'@

3. @(.) g = g `seq` \ f -> f `seq` \ x -> g (f x)@
This fails only the first (application) identity:
@(const () . undefined) () `seq` ()  =  'Prelude.undefined' -- should be ()@

Unfortunately, there's no way to satisfy all three. You'd need to apply @g@ to the whole expression before forcing @f@, but force @f@ before taking the @x@ parameter.

Version 2 satisfies as many goals as 3 while satisfying the goal satisfied by 'Prelude..', so 2 is what I've gone with. However, the 'Functor' laws are @'fmap' 'id' = 'id'@ and @'fmap' g . 'fmap' (g . f)  =  'fmap' g . 'fmap' f@. This means that, for @Functor ((->) c)@, 'fmap' should be version 3.
-}
(.) g = g `seq` \ f x -> g (f x)

infixr 0 $
($) :: forall ra rb (a :: TYPE ra) (b :: TYPE rb). (a -> b) -> a -> b
{-^ Prelude's '$' is defined as @f $ x = f x@. That does not preserve eta-equivalence, so here it's redefined as a specialization of 'id': @($) f = f@.
-}
($) f = f

infixr 0 $!
($!) :: forall r a (b :: TYPE r). (a -> b) -> a -> b
{-^ @($!) f@ makes @f@ strict in its first argument.
@($!) 'Prelude.undefined' = 'Prelude.undefined'@
@('Prelude.undefined' $!) `seq` () = 'Prelude.undefined' -- with PostfixOperators@
@('Prelude.undefined' $!) `seq` () = () -- without PostfixOperators@
-}
($!) f = f `seq` \ x -> let !x' = x in f x'

infixl 1 &
(&) :: forall r a (b :: TYPE r). a -> (a -> b) -> b
{-^ @x & f = f x@
@y & x & g = g (x y)@ -- not @g x y@!

>>> (& 'Prelude.undefined') `seq` ()
()
-}
x & f = f x

-- infixl 1 !&
-- (!&) :: a -> (a -> b) -> b
-- {-^ Evaluate a value, then apply a function to it.
-- @y !& x !& g = g $! x $! y@ -- not @y !& (x !& g)@!
-- @(!& 'Prelude.undefined') `seq` () = ()@
-- -}
-- x !& f = x `seq` f x
-- {-# INLINABLE (!&) #-}

curry :: forall r a b (c :: TYPE r). ((a, b) -> c) -> a -> b -> c
{-^ @curry f x y = f (x, y)@
@curry 'Prelude.undefined' = 'Prelude.undefined'@
-}
curry f = f `seq` \ x y -> f (x, y)

uncurry :: forall r a b (c :: TYPE r). (a -> b -> c) -> (a, b) -> c
{-^ @uncurry f (x, y) = f x y@
@uncurry 'Prelude.undefined' = 'Prelude.undefined'@
Currently, @uncurry f 'Prelude.undefined' = f 'Prelude.undefined' 'Prelude.undefined'@. This matches Data.Tuple.uncurry. Is this the "correct" behavior?
-}
uncurry f = f `seq` \ ~(x, y) -> f x y

flip :: forall r a b (c :: TYPE r). (a -> b -> c) -> b -> a -> c
{-^ @flip f y x = f x y@
-}
flip f = f `seq` \ y x -> f x y

infixl 0 `on`
on :: forall r a b (c :: TYPE r). (b -> b -> c) -> (a -> b) -> a -> a -> c
{-^ @on f g x y = f (g x) (g y)@
@g `on` 'id'  =  g@
@on 'Prelude.undefined' = 'Prelude.undefined'@
@('Prelude.undefined' `on`) `seq` () = 'Prelude.undefined'@ -- with PostfixOperators
@('Prelude.undefined' `on`) `seq` () = ()@ -- without PostfixOperators
-}
on g = g `seq` \ f x y -> g (f x) (f y)
