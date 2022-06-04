{-# LANGUAGE PostfixOperators #-}

import Prelude hiding ((.), ($), ($!), flip, curry, uncurry)
import Control.Applicative (liftA2)
import Control.Monad (unless)
import Control.Exception as Except
import qualified System.IO.Unsafe as Unsafe
import Eta

main :: IO ()
main =
    checkUndefined undefined "undefined" <&&>
    checkUndefined ((.) undefined) "(.) undefined" <&&>
    checkUndefined (undefined .) "(undefined .)" <&&>
    checkUndefined (($) undefined) "($) undefined" <&&>
    checkUndefined (undefined $) "(undefined $)" <&&>
    checkUndefined (($!) undefined) "($!) undefined" <&&>
    checkUndefined (undefined $!) "(undefined $!)" <&&>
    checkUndefined (flip undefined) "flip undefined" <&&>
    checkUndefined (curry undefined) "curry undefined" <&&>
    checkUndefined (uncurry undefined) "uncurry undefined" <&&>
    checkUndefined (on undefined) "on undefined" >>=
    flip unless (error "Test suite failed.")


checkUndefined x s = isBottom x >>= \ b -> if b
    then pure True
    else False <$ putStrLn ("'" <> s <> "' is not 'undefined'")


infixl 4 <&&>
(<&&>) :: Applicative m => m Bool -> m Bool -> m Bool
(<&&>) = liftA2 (&&)

isBottom :: a -> IO Bool
isBottom x =
   (False <$ Except.evaluate x) `Except.catch` \ Except.SomeException{} -> pure True
   -- Except.try (Except.evaluate x) >>= \ e -> case e of
   -- Left Except.SomeException{} -> pure True
   -- Right{} -> pure False
