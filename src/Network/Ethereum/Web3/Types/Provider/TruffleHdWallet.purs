module Network.Ethereum.Web3.Types.Provider.TruffleHdWallet where

import Data.Maybe (Maybe)
import Effect (Effect)
import Network.Ethereum.Web3 (Provider)

foreign import hdWalletProvider :: 
  String -> 
  String -> 
  Maybe Int ->
  Maybe Int ->
  Maybe Boolean ->
  Maybe String ->
  Effect Provider
