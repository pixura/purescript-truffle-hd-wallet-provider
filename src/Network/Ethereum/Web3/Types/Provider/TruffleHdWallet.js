"use strict";
const HttpProvider = require("@truffle/hdwallet-provider");

exports.hdWalletProvider = function(mnemonic, provider, addressIndex, numAddresses, shareNonce, walletHdpath) {
  return new HDWalletProvider(mnemonic, provider, addressIndex, numAddresses, shareNonce, walletHdpath);

}