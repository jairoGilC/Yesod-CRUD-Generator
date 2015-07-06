{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Crud.Core where

-- import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.Data
import Import
import Yesod.Form.Bootstrap3
import Text.Blaze


getNew :: String -> String -> TH.Q [TH.Dec]
getNew name formName = do
  let method = TH.mkName $ "get" ++ name ++ "NewR"
      form = TH.varE $ TH.mkName formName
      action = TH.conE $ TH.mkName $ name ++ "NewR"
      body = [| do
                 (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $form Nothing
                 defaultLayout $ do
                    let actionR = $action                          
                    formHamlet widget encoding actionR |]
  typ <- TH.sigD method [t|  HandlerT App IO Html |]
  fun <- TH.funD method [TH.clause [] (TH.normalB body) []]
  return [typ,fun]


postNew :: String -> String -> String ->  TH.Q [TH.Dec]
postNew name formName redirectName = do
           let method = TH.mkName $ "post" ++ name ++ "NewR"
               form = TH.varE $ TH.mkName formName            
               action = TH.conE $ TH.mkName $ name ++ "NewR"
               redirectAction = TH.conE $ TH.mkName redirectName
               body = [| do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  Nothing
                case result of
                     FormSuccess entity -> do 
                                 _ <- runDB $ insert entity
                                 redirect $redirectAction
                     _ -> defaultLayout $ do
                     let actionR = $action                
                     formHamlet widget encoding actionR|]  
           typ <- TH.sigD method [t|  HandlerT App IO Html |]          
           fun <- TH.funD method [TH.clause [] (TH.normalB body) []]
           return [typ,fun]

getEdit :: String -> String -> TH.Q [TH.Dec]
getEdit name formName = do
  entityName <- TH.newName "eId"
  let method = TH.mkName $ "get" ++ name ++ "EditR"
      form = TH.varE $ TH.mkName formName
      action = TH.conE $ TH.mkName $ name ++ "EditR"
      entityId = TH.varE entityName
      entityParam = TH.varP entityName
      entityType  = TH.conT $ TH.mkName $ name ++ "Id"
      body = [| do 
             entity <- runDB $ get404 $entityId  
             (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  (Just entity)
             defaultLayout $ do
               let actionR = $action $entityId
               formHamlet widget encoding actionR |]
  typ <- TH.sigD method [t| $entityType ->  HandlerT App IO Html |]   
  fun <- TH.funD method [TH.clause [entityParam] (TH.normalB body) []]
  return [typ,fun]

postEdit :: String -> String -> String -> TH.Q [TH.Dec]
postEdit name formName redirectName = do
  entityName <- TH.newName "eId"
  let method = TH.mkName $ "post" ++ name ++ "EditR"
      form = TH.varE $ TH.mkName formName
      action = TH.conE $ TH.mkName $ name ++ "EditR"
      entityId = TH.varE entityName
      entityParam = TH.varP entityName
      entityType  = TH.conT $ TH.mkName $ name ++ "Id"
      redirectAction = TH.conE $ TH.mkName redirectName      
      body =    [|  do 
                entity <- runDB $ get404 $entityId
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  (Just entity)
                case result of
                     FormSuccess result -> do 
                                 _ <- runDB $ replace $entityId  result
                                 redirect $redirectAction
                     _ -> defaultLayout $ do     
                     let actionR = $action $entityId                          
                     formHamlet widget encoding actionR |]
  typ <- TH.sigD method [t| $entityType ->  HandlerT App IO Html |]         
  fun <- TH.funD method [TH.clause [entityParam] (TH.normalB body) []]
  return [typ,fun]

deleteCrud :: String  -> String ->  TH.Q [TH.Dec]
deleteCrud name  redirectName=  do
  entityName <- TH.newName "eId"
  let method = TH.mkName $ "delete" ++ name ++ "DeleteR"
      entityId = TH.varE entityName
      entityParam = TH.varP entityName
      entityType  = TH.conT $ TH.mkName $ name ++ "Id"
      redirectAction = TH.conE $ TH.mkName redirectName      
      body = [| do 
             runDB $ delete $entityId
             redirect $redirectAction |]
  typ <- TH.sigD method [t| $entityType ->  HandlerT App IO Html |] 
  fun <- TH.funD method [TH.clause [entityParam] (TH.normalB body) []]
  return [typ,fun]

listCrud :: String  -> TH.Q [TH.Dec]
listCrud name =  do  
  let method = TH.mkName $ "get" ++ name ++ "ListR"          
      body = [| do  list <- runDB $ selectList [] []                   
                    defaultLayout $ do
                       $(widgetFile "Demo/DemoList")|]  
  typ <- TH.sigD method [t|  HandlerT App IO Html |]    
  fun <- TH.funD method [TH.clause [] (TH.normalB body) []]
  return [typ,fun]

formHamlet :: forall site (m :: * -> *) a a1.
                    (ToMarkup a, MonadThrow m,
                     MonadBaseControl IO m, MonadIO m, ToWidget site a1) =>
                    a1 -> a -> Route site -> WidgetT site m ()
formHamlet widget encoding actionR = [whamlet|
<div .container>
    <form method=post action=@{actionR} encType=#{encoding}>
         <div .row>
             ^{widget}   
            <div .row .clearfix .visible-xs-block>
            <button .btn .btn-success> 
               <span .glyphicon .glyphicon-floppy-saved>
               submit
|]
