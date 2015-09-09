module Handler.Home where

import Import
import Crud.Core
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              withSmallInput)

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    aDomId <- newIdent
    defaultLayout $ do
        $(widgetFile "homepage")


 
--Example 3
{-
rssForm :: Maybe Rss -> AForm Handler Rss
rssForm   rss = Rss 
                <$> areq textField "contenido" (rssContenido <$> rss)
                <*> areq textField "titulo" (rssTitulo <$> rss) 
                <*> areq textField "url" (rssUrl <$> rss) 



getNewRoute   "Rss" "rssForm"
postNewRoute  "Rss" "rssForm" "RssListR"
getEditRoute  "Rss" "rssForm"
postEditRoute "Rss" "rssForm" "RssListR"

deleteCrudRoute "Rss" "RssListR"
listCrudRoute "Rss" "rssContenido"
-}