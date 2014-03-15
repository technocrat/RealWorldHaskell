{-# LINE 1 "Regex.hsc" #-}
module Chapter17.Regex where
{-# LINE 2 "Regex.hsc" #-}

import           Data.Bits       ((.|.))
import           Foreign.C.Types


{-# LINE 7 "Regex.hsc" #-}

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

caseless        :: PCREOption
caseless        = PCREOption 1
dollar_endonly  :: PCREOption
dollar_endonly  = PCREOption 32
dotall          :: PCREOption
dotall          = PCREOption 4

{-# LINE 29 "Regex.hsc" #-}

-- | Combine a list of options into a single option, using bitwise (.|.)
combineOptions :: [PCREOption] -> PCREOption
combineOptions = PCREOption . foldr ((.|.) . unPCREOption) 0
