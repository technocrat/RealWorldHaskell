module Chapter14.Carrier where

import qualified Data.Map as M
import           Prelude  hiding (lookup)

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Netwrok
                   | Morrisas_Marvelous_Mobiles
                   | Petes_Plutocratic_Phones
                     deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                          -> M.Map PersonName PhoneNumber
                          -> M.Map PhoneNumber MobileCarrier
                          -> M.Map MobileCarrier BillingAddress
                          -> Maybe BillingAddress
findCarrierBillingAddress = undefined

variation1 :: PersonName
           -> M.Map PersonName PhoneNumber
           -> M.Map PhoneNumber MobileCarrier
           -> M.Map MobileCarrier BillingAddress
           -> Maybe BillingAddress
variation1 person phoneMap carrierMap addressMap =
    case M.lookup person phoneMap of
      Nothing     -> Nothing
      Just number -> case M.lookup number carrierMap of
                       Nothing      -> Nothing
                       Just carrier -> M.lookup carrier addressMap

variation2 :: PersonName
           -> M.Map PersonName PhoneNumber
           -> M.Map PhoneNumber MobileCarrier
           -> M.Map MobileCarrier BillingAddress
           -> Maybe BillingAddress
variation2 person phoneMap carrierMap addressMap = do
  number  <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  address <- M.lookup carrier addressMap
  return address

variation2a :: PersonName
            -> M.Map PersonName PhoneNumber
            -> M.Map PhoneNumber MobileCarrier
            -> M.Map MobileCarrier BillingAddress
            -> Maybe BillingAddress
variation2a person phoneMap carrierMap addressMap = do
  number  <- M.lookup person phoneMap
  carrier <- M.lookup number carrierMap
  M.lookup carrier addressMap

variation3 :: PersonName
           -> M.Map PersonName PhoneNumber
           -> M.Map PhoneNumber MobileCarrier
           -> M.Map MobileCarrier BillingAddress
           -> Maybe BillingAddress
variation3 person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
    where lookup :: (Ord k) => M.Map k a -> k -> Maybe a
          lookup = flip M.lookup
