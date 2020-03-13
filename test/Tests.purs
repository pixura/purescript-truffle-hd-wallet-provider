module Tests where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Control.Monad.Reader (ask)
import Data.Array.Partial (head)
import Data.Either (Either(..), fromRight, isRight)
import Data.Lens ((?~))
import Data.Maybe (fromJust)
import Data.Nullable (null)
import Data.Ord (abs)
import Effect.Aff (Aff, Error, Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Network.Ethereum.Core.BigNumber (BigNumber, decimal, parseBigNumber)
import Network.Ethereum.Core.HexString (HexString, mkHexString)
import Network.Ethereum.Web3 (Address, BlockNumber(..), ChainCursor(..), Provider, Szabo, Transaction(..), TransactionOptions(..), TransactionReceipt(..), TransactionStatus(..), Value, Web3, _from, _gas, _gasPrice, _to, _value, defaultTransactionOptions, embed, fromMinorUnit, httpProvider, mkAddress, mkValue, runWeb3, toMinorUnit)
import Network.Ethereum.Web3.Api (eth_getAccounts, eth_getBalance, eth_getTransaction, eth_getTransactionReceipt, eth_sendTransaction)
import Network.Ethereum.Web3.Types (NoPay)
import Network.Ethereum.Web3.Types.HdWalletProvider (hdWalletProvider, unHdWalletProvider)
import Partial.Unsafe (unsafeCrashWith, unsafePartial)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual, shouldSatisfy)

-----------------------------------------------------------------------------
-- | spec
-----------------------------------------------------------------------------
spec :: SpecT Aff Unit Aff Unit
spec = describe "Tests" do
  let 
    mnemonic = "glory style layer ready document pattern anxiety rather ladder pony chronic use similar feature buzz all resist banana deny rescue viable sibling faith early"
    expectedAddress = unsafePartial fromJust $ (mkAddress =<< mkHexString "0xab455cc1aa1b3e96f15ece23cfc650ee926dff18")
    rpc = "http://localhost:8545" 
  httpProv <- liftEffect $ httpProvider rpc
  hdprov <- liftEffect $ hdWalletProvider {mnemonic, rpc, numberOfAccounts: null, path:null}
  it "can derive the correct account from a seed phrase" do
    accs <- withWeb3 (unHdWalletProvider hdprov) eth_getAccounts
    accs `shouldEqual` [expectedAddress]
  it "can send money" do
    richAccount <- unsafePartial head <$> withWeb3 httpProv eth_getAccounts
    let (unitPrice :: Value Szabo) = mkValue $ embed $ 10000
        (unitPriceSend :: Value Szabo) = mkValue $ embed $ 5000
        bnPrice = toMinorUnit unitPrice
        bnPriceSend = toMinorUnit unitPriceSend
        txOptsRec = defaultTxOpts richAccount # _value ?~ mkValue (toMinorUnit unitPrice) # _to ?~ expectedAddress
        txOptsSend = defaultTxOpts expectedAddress # _value ?~ mkValue (toMinorUnit unitPriceSend) # _to ?~ richAccount
    txHashReceive <- withWeb3 httpProv $ eth_sendTransaction txOptsRec
    withWeb3 httpProv $ awaitTxSuccessWeb3 txHashReceive
    withWeb3 (unHdWalletProvider hdprov) $ checkEthDifference expectedAddress bnPrice txHashReceive
    txHashSend <- withWeb3 (unHdWalletProvider hdprov) $ eth_sendTransaction txOptsSend
    withWeb3 (unHdWalletProvider hdprov) $ checkEthDifference expectedAddress bnPriceSend txHashSend


-----------------------------------------------------------------------------
-- | spec
-----------------------------------------------------------------------------
defaultTxOpts :: Address -> TransactionOptions NoPay
defaultTxOpts primaryAccount =
  let
    limit = unsafePartial fromJust $ parseBigNumber decimal "6712388"

    price = unsafePartial fromJust $ parseBigNumber decimal "10000000"
  in
    defaultTransactionOptions # _from ?~ primaryAccount
      # _gas
      ?~ limit
      # _gasPrice
      ?~ price

withWeb3 ::
  forall m a.
  Show a =>
  MonadAff m =>
  MonadThrow Error m => Provider -> Web3 a -> m a
withWeb3 prov f = do
  res <- liftAff $ runWeb3 prov f
  res `shouldSatisfy` isRight
  pure $ unsafePartial fromRight res

awaitTxSuccess :: forall m. MonadAff m => HexString -> Provider -> m Unit
awaitTxSuccess txHash provider = do
  TransactionReceipt txReceipt <- pollTransactionReceipt txHash provider
  case txReceipt.status of
    Succeeded -> pure unit
    Failed -> unsafeCrashWith $ "Transaction Failed w/ hash " <> show txHash <> "\n" <> show txReceipt

awaitTxSuccessWeb3 :: HexString -> Web3 Unit
awaitTxSuccessWeb3 txHash = awaitTxSuccess txHash =<< ask


-----------------------------------------------------------------------------
-- | checkEthDifference
-----------------------------------------------------------------------------
checkEthDifference :: Address -> BigNumber -> HexString -> Web3 Unit
checkEthDifference addr diff txHash = do
  { blockNumber, gasUsed, gasPrice, from, to } <- getTxDetails
  let
    (BlockNumber blockNumBN) = blockNumber

    weiSpentOnGas = gasPrice * gasUsed
  balanceAfter <- getBalance $ BN blockNumber
  balanceBefore <- getBalance $ BN $ BlockNumber $ blockNumBN - embed 1
  let
    gasCostFactor = if balanceAfter > balanceBefore then embed (-1) else embed 1

    diffWithGas = diff + if from == addr then gasCostFactor * weiSpentOnGas else embed 0
  abs (balanceAfter - balanceBefore) `shouldEqual` diffWithGas
  where
  getBalance = eth_getBalance addr

  getTxDetails = do
    (Transaction { gasPrice, from, to }) <- eth_getTransaction txHash
    (TransactionReceipt { blockNumber, gasUsed }) <- eth_getTransactionReceipt txHash
    pure { blockNumber, gasPrice, gasUsed, from, to }

-----------------------------------------------------------------------------
-- | pollTransactionReceipt
-----------------------------------------------------------------------------
pollTransactionReceipt
  :: forall m.
     MonadAff m
  => HexString
  -> Provider
  -> m TransactionReceipt
pollTransactionReceipt txHash provider = do
  etxReceipt <- liftAff <<< runWeb3 provider $ eth_getTransactionReceipt txHash
  case etxReceipt of
    Left _ -> do
      liftAff $ delay (Milliseconds 3000.0)
      pollTransactionReceipt txHash provider
    Right txRec -> pure txRec