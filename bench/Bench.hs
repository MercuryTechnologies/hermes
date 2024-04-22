module Bench where

import Criterion.Main

main :: IO ()
main = defaultMain
  [ bgroup "bench"
    [ bench "bench" $ whnfIO $ pure ()
    ]
  ]

  