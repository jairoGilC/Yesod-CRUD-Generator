{-# LANGUAGE DeriveDataTypeable #-}

module Crud.Core where

-- import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.Data
import Import
import Yesod.Form.Bootstrap3



getNew :: String -> String -> TH.Q [TH.Dec]
getNew name formName = do
           let method = TH.mkName $ "get" ++ name ++ "NewR"
               form = TH.varE $ TH.mkName formName
               action = TH.conE $ TH.mkName $ name ++ "NewR"
               body = [| do
                 (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $form Nothing
                 defaultLayout $ do
                    let actionR = $action                          
                    $(widgetFile "Demo/DemoCreate") |]
           (:[]) <$> TH.funD method [TH.clause [] (TH.normalB body) []]


postNew :: String -> String -> TH.Q [TH.Dec]
postNew name formName = do
           let method = TH.mkName $ "post" ++ name ++ "NewR"
               form = TH.varE $ TH.mkName formName            
               action = TH.conE $ TH.mkName $ name ++ "NewR"
               body = [| do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  Nothing
                case result of
                     FormSuccess entity -> do 
                                 _ <- runDB $ insert entity
                                 redirect DemoListR
                     _ -> defaultLayout $ do
                     let actionR = $action                
                     $(widgetFile "Demo/DemoCreate")|]  
           (:[]) <$> TH.funD method [TH.clause [] (TH.normalB body) []]

getEdit :: String -> String -> TH.Q [TH.Dec]
getEdit name formName = do
  entityName <- TH.newName "eId"
  let method = TH.mkName $ "get" ++ name ++ "EditR"
      form = TH.varE $ TH.mkName formName
      action = TH.conE $ TH.mkName $ name ++ "EditR"
      entityId = TH.varE entityName
      entityParam = TH.varP entityName
      body = [| do 
             entity <- runDB $ get404 $entityId  
             (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  (Just entity)
             defaultLayout $ do
               let actionR = $action $entityId
               $(widgetFile "Demo/DemoCreate") |]
  (:[]) <$> TH.funD method [TH.clause [entityParam] (TH.normalB body) []]

postEdit :: String -> String -> TH.Q [TH.Dec]
postEdit name formName = do
  entityName <- TH.newName "eId"
  let method = TH.mkName $ "post" ++ name ++ "EditR"
      form = TH.varE $ TH.mkName formName
      action = TH.conE $ TH.mkName $ name ++ "EditR"
      entityId = TH.varE entityName
      entityParam = TH.varP entityName
      body =    [|  do 
                entity <- runDB $ get404 $entityId
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  (Just entity)
                case result of
                     FormSuccess result -> do 
                                 _ <- runDB $ replace $entityId  result
                                 redirect DemoListR
                     _ -> defaultLayout $ do     
                     let actionR = $action $entityId                          
                     $(widgetFile "Demo/DemoCreate") |]
  (:[]) <$> TH.funD method [TH.clause [entityParam] (TH.normalB body) []]

deleteCrud :: String  -> TH.Q [TH.Dec]
deleteCrud name  =  do
  entityName <- TH.newName "eId"
  let method = TH.mkName $ "delete" ++ name ++ "DeleteR"
      entityId = TH.varE entityName
      entityParam = TH.varP entityName            
      body = [| do 
             runDB $ delete $entityId
             redirect DemoListR |]
  (:[]) <$> TH.funD method [TH.clause [entityParam] (TH.normalB body) []]


listCrud :: String  -> TH.Q [TH.Dec]
listCrud name =  do  
  let method = TH.mkName $  name ++ "ListR"          
      body = [| do  demos <- runDB $ selectList [] []                   
                    defaultLayout $ do
                       $(widgetFile "Demo/DemoList")|]                    
  (:[]) <$> TH.funD method [TH.clause [] (TH.normalB body) []]



