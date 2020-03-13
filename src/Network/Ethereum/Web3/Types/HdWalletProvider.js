"use strict";
const HDWalletProvider = require("@truffle/hdwallet-provider");

exports.hdWalletProviderImpl = function({ mnemonic, rpc, numberOfAccounts, path }) {
  return new HDWalletProvider( mnemonic, rpc, undefined, numberOfAccounts || 1, true, path || undefined );
};
