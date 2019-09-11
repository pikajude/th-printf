{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Haskell.Printf
  ( -- $intro
    s
  , t
  , p
  , hp
  )
where

import           Control.Applicative            ( pure )
import           Control.Monad.IO.Class
import           Language.Haskell.Printf.Lib
import           Language.Haskell.TH.Lib
import           Language.Haskell.TH.Quote
import           Language.Haskell.TH.Syntax
import           System.IO                      ( hPutStr )

-- | @
-- ['s'|Hello, %s! (%d people greeted)|] :: ... -> 'String'
-- @
--
-- This formatter follows the guidelines listed
-- <http://www.cplusplus.com/reference/cstdio/printf/ here>, except for
-- @%n@ (store number of printed characters) for obvious
-- reasons.
--
-- @
-- %c     :: 'Char'
-- %s     :: 'String'
--
-- -- datatypes with Show instances
-- %?     :: 'Show' a => a
--
-- -- signed integer types
-- %d, %i :: 'Integral' i => i
--
-- -- unsigned integer types
-- %u     :: ('Bounded' i, 'Integral' i) => i
-- %o     :: ('Bounded' i, 'Integral' i) => i
-- %x, %X :: ('Bounded' i, 'Integral' i) => i
--
-- -- floats
-- %a, %A :: 'RealFloat' f => f
-- %e, %E :: 'RealFloat' f => f
-- %f, %F :: 'RealFloat' f => f
-- %g, %G :: 'RealFloat' f => f
--
-- %p     :: 'Foreign.Ptr.Ptr' a
-- @
s :: QuasiQuoter
s = quoter $ \s' -> do
  (lhss, rhs) <- toSplices s' OutputString
  return $ LamE lhss rhs

-- | Behaves identically to 's', but produces lazy 'Data.Text.Lazy.Text'.
t :: QuasiQuoter
t = quoter $ \s' -> do
  (lhss, rhs) <- toSplices s' OutputText
  return $ LamE lhss rhs

-- | Like 's', but prints the resulting string to @stdout@.
--
-- @
-- [p|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => ... -> m ()
-- @
p :: QuasiQuoter
p = quoter $ \s' -> do
  (lhss, rhs) <- toSplices s' OutputString
  lamE (map pure lhss) [|liftIO (putStr $(pure rhs))|]

-- | Like 'p', but takes as its first argument the 'System.IO.Handle' to print to.
--
-- @
-- [hp|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => 'System.IO.Handle' -> ... -> m ()
-- @
hp :: QuasiQuoter
hp = quoter $ \s' -> do
  (lhss, rhs) <- toSplices s' OutputString
  h           <- newName "h"
  lamE (varP h : map pure lhss) [|liftIO (hPutStr $(varE h) $(pure rhs))|]

quoter :: (String -> ExpQ) -> QuasiQuoter
quoter e = QuasiQuoter
  { quoteExp  = e
  , quotePat  = error "this quoter cannot be used in a pattern context"
  , quoteType = error "this quoter cannot be used in a type context"
  , quoteDec  = error "this quoter cannot be used in a declaration context"
  }

-- $intro
-- "Text.Printf" is a useful module, but due to the typeclass hacks it uses, it can
-- be hard to tell if the format string you wrote is well-formed or not.
-- This package provides a mechanism to create formatting functions at compile time.
--
-- Note that, to maintain consistency with other printf implementations, negative ints
-- that are printed as unsigned will \"underflow\".
--
-- >>> [s|%u|] (-1 :: Int32)
-- "4294967295"
--
-- Thus, any time you want to print a number using the unsigned, octal, or hex specifiers,
-- your input must be an instance of "Bounded".
