module Api where

import Prelude

import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX(), get)
import Network.HTTP.StatusCode (StatusCode(..))

fetchResults :: forall eff. String -> Aff (ajax :: AJAX | eff) String
fetchResults q = do
    result <- get $ "http://localhost:8080/results?q=" <> q
    return case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"

fetchDetails :: forall eff. Int -> Aff (ajax :: AJAX | eff) String
fetchDetails id = do
    result <- get $ "http://localhost:8080/details/" <> show id
    return case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"
