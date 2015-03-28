{-# LANGUAGE OverloadedStrings #-}
import Web.Scotty
import Network.Wai.Middleware.Static

main :: IO ()
main = scotty 3000 $ do

  get "/" $ file "dist/index.html"

  middleware $ staticPolicy (noDots >-> addBase "dist")

