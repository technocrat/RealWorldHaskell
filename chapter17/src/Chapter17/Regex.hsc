module Chapter17.Regex where

import           Data.Bits       ((.|.))
import           Foreign.C.Types

#include <pcre.h>

-- | A type for PCRE compile-time options. These are newtyped CInts,
-- which can be bitwise-or'd together, using '(Data.Bits..|.)'
newtype PCREOption = PCREOption { unPCREOption :: CInt }
    deriving (Eq, Show)

{-
caseless :: PCREOption
caseless = PCREOption #const PCRE_CASELESS

dollar_endonly :: PCREOption
dollar_endonly = PCREOption #const PCRE_DOLLAR_ENDONLY

dotall :: PCREOption
dotall = PCREOption #const PCRE_DOTAL
-}

#{enum PCREOption, PCREOption
 , caseless       = PCRE_CASELESS
 , dollar_endonly = PCRE_DOLLAR_ENDONLY
 , dotall         = PCRE_DOTALL
 }

-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
