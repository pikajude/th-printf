{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.Haskell.Printf
    ( s
    , p
    , hp
    ) where

import Control.Monad.IO.Class
import Language.Haskell.Printf.Lib
import Language.Haskell.TH.Lib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import System.IO (stdout, hPutStr)

-- | @
-- ['s'|Hello, %s! (%d people greeted)|] :: ... -> 'S.Str'
-- @
--
-- This formatter follows the guidelines listed
-- <http://www.cplusplus.com/reference/cstdio/printf/ here>, except for
-- @%n@ (store number of printed characters) for obvious
-- reasons.
--
-- @
-- %c     :: 'S.Chr'
-- %s     :: 'S.Str'
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
--
-- N.B.: For consistency with other @printf@ implementations, arguments formatted as
-- unsigned integer types will \"underflow\" if negative.
s :: QuasiQuoter
s =
    quoter
        { quoteExp =
              \s' -> do
                  (lhss, rhs) <- toSplices s'
                  return $ LamE lhss rhs
        }

-- | Like 's', but prints the resulting string to @stdout@.
--
-- @
-- [p|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => ... -> m ()
-- @
p :: QuasiQuoter
p =
    quoter
        { quoteExp =
              \s' -> do
                  (lhss, rhs) <- toSplices s'
                  lamE (map pure lhss) [|liftIO (hPutStr stdout $(pure rhs))|]
        }

-- | Like 'p', but takes as its first argument the 'System.IO.Handle' to print to.
--
-- @
-- [hp|Hello, %s! (%d people greeted)|] :: 'MonadIO' m => 'System.IO.Handle' -> ... -> m ()
-- @
hp :: QuasiQuoter
hp =
    quoter
        { quoteExp =
              \s' -> do
                  (lhss, rhs) <- toSplices s'
                  h <- newName "h"
                  lamE
                      (varP h : map pure lhss)
                      [|liftIO (hPutStr $(varE h) $(pure rhs))|]
        }

quoter :: QuasiQuoter
quoter =
    QuasiQuoter
        { quoteExp = undefined
        , quotePat = error "this quoter cannot be used in a pattern context"
        , quoteType = error "this quoter cannot be used in a type context"
        , quoteDec = error "this quoter cannot be used in a declaration context"
        }
