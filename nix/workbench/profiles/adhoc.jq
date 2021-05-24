def adhoc_profiles:
[ { name: "short"
  , generator: { tx_count: 10000, inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100 }
  }
, { name: "small"
  , generator: { tx_count: 1000,  inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100
               , init_cooldown: 25 }
  , tolerances: { finish_patience: 4 }
  }
, { name: "smoke"
  , generator: { tx_count: 100,   add_tx_size: 0, inputs_per_tx: 1, outputs_per_tx: 1,  tps: 100
               , init_cooldown: 25 }
  , tolerances: { finish_patience: 4 }
  }
, { name: "default"
  , genesis:
    { verbatim:
      { initialFunds:
        # genesis-utxo (used for pool owner funds and paying fees for startup scripts)
        { "608634fc2a05c3d7f8dca90321dae19a2172ab3ff146512660721fa15b": 1000000000000000
        # flee three produce crush token where quantum vessel seek include dance reject urge awesome lonely
        , "00da45b43746dec67dfe3647f3dfee9fb8bf0723883e40ca14b51234c8e62cf6242003415534e919acb2c07132a588827fb8e6b968861b1e5d807b756fe72d6e28": 100000000000000
        , "0036af4eaf83f34348829c0b88515238e3bbe7c7b69a5b3659bc3155521da98aa12003415534e919acb2c07132a588827fb8e6b968861b1e5d807b756fe72d6e28": 100000000000000
        , "00bee681d5f0dbd1b047c0311ab1211640f5f862c614d5db9e21fc08ddb163aba02003415534e919acb2c07132a588827fb8e6b968861b1e5d807b756fe72d6e28": 100000000000000
        , "00b84e9b8980d61ade5167f3e697641acae5e6eec546fcdbc5c03149a02595d4202003415534e919acb2c07132a588827fb8e6b968861b1e5d807b756fe72d6e28": 100000000000000
        , "000b04a2d5dc696f845a7bbe7ead094ac12e8dbbf82eedeabe9cb2fe8c07f1624c2003415534e919acb2c07132a588827fb8e6b968861b1e5d807b756fe72d6e28": 100000000000000
        }
      , activeSlotsCoeff: 0.1
      , epochLength: 1000
      , slotLength: 0.2
      , securityParam: 10
      }
      , genesis_future_offset: "10 seconds"
      , utxo:                  0
    }
  }
];
