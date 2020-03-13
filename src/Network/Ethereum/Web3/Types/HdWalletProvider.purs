module Network.Ethereum.Web3.Types.HdWalletProvider
  ( hdWalletProvider
  , HdWalletProvider(..)
  , unHdWalletProvider
  ) where

import Prelude
import Data.Nullable (Nullable)
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Network.Ethereum.Web3 (Provider)

newtype HdWalletProvider
  = HdWalletProvider Provider

unHdWalletProvider :: HdWalletProvider -> Provider
unHdWalletProvider (HdWalletProvider p) = p

foreign import hdWalletProviderImpl ::
  EffectFn1
    { mnemonic :: String
    , rpc :: String
    , numberOfAccounts :: Nullable Int
    , path :: Nullable String
    }
    Provider

hdWalletProvider ::
  { mnemonic :: String
  , rpc :: String
  , numberOfAccounts :: Nullable Int
  , path :: Nullable String
  } ->
  Effect HdWalletProvider
hdWalletProvider = map HdWalletProvider <<< runEffectFn1 hdWalletProviderImpl
