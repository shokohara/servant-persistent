{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators              #-}

module Api.Article where

import           Control.Monad.Except
import           Control.Monad.Reader        (ReaderT, runReaderT)
import           Control.Monad.Reader.Class
import           Data.Int                    (Int64)
import           Database.Persist.Postgresql (Entity (..), fromSqlKey, insert,
                                              selectFirst, selectList, (==.))
import           Network.Wai                 (Application)
import           Servant
import           Servant.JS                  (vanillaJS, writeJSForAPI)

import           Config                      (App (..), Config (..))
import           Models

type UserAPI =
         "articles" :> Get '[JSON] [Entity Article]
    :<|> "articles" :> Capture "name" String :> Get '[JSON] (Entity Article)
    :<|> "articles" :> ReqBody '[JSON] Article :> Post '[JSON] Int64

-- | The server that runs the UserAPI
userServer :: ServerT UserAPI App
userServer = allUsers :<|> singleUser :<|> createUser

-- | Returns all users in the database.
allUsers :: App [Entity Article]
allUsers =
    runDb (selectList [] [])

-- | Returns a user by name or throws a 404 error.
singleUser :: String -> App (Entity Article)
singleUser str = do
    maybeUser <- runDb (selectFirst [ArticleName ==. str] [])
    case maybeUser of
         Nothing ->
            throwError err404
         Just person ->
            return person

-- | Creates a user in the database.
createUser :: Article -> App Int64
createUser p = do
    newUser <- runDb (insert (Article (articleName p) (articleEmail p)))
    return $ fromSqlKey newUser

-- | Generates JavaScript to query the User API.
generateJavaScript :: IO ()
generateJavaScript =
    writeJSForAPI (Proxy :: Proxy UserAPI) vanillaJS "./assets/api.js"
