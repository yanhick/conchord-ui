module Api where

import Prelude

import Control.Monad.Aff (Aff())
import Network.HTTP.Affjax (AJAX(), get)
import Network.HTTP.StatusCode (StatusCode(..))

fetchResults :: forall eff. Aff (ajax :: AJAX | eff) String
fetchResults = do
    result <- get "http://localhost:8080/results"
    return case result.status of
             (StatusCode 200) -> result.response
             _ -> "fail"
