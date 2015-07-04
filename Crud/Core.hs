{-# LANGUAGE DeriveDataTypeable #-}

module Crud.Core where

-- import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.Data
import Import
import Yesod.Form.Bootstrap3


-- demoId = "demoId"

getNew :: String -> String -> TH.Q [TH.Dec]
getNew name formName = do
           let method = TH.mkName $ "get" ++ name ++ "NewR"
               form = TH.varE $ TH.mkName formName
           body <- [| do 
             (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $form Nothing
             defaultLayout $ do
                let actionR = DemoNewR                          
                $(widgetFile "Demo/DemoCreate") |]
           (:[]) <$> TH.funD method [return (TH.Clause [] (TH.NormalB body) [])]


postNew :: String -> String -> TH.Q [TH.Dec]
postNew name formName = do
           let method = TH.mkName $ "post" ++ name ++ "NewR"
               form = TH.varE $ TH.mkName formName            
           body <- [| do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  Nothing
                case result of
                     FormSuccess demo -> do 
                                 _ <- runDB $ insert demo
                                 redirect DemoListR
                     _ -> defaultLayout $ do
                     let actionR = DemoNewR                
                     $(widgetFile "Demo/DemoCreate")|]  
           (:[]) <$> TH.funD method [return (TH.Clause [] (TH.NormalB body) [])]

-- getEdit :: String -> Key -> String -> TH.Q [TH.Dec]
getEdit :: String -> String -> String -> TH.Q [TH.Dec]
getEdit name nameId formName = do
           let method = TH.mkName $ "get" ++ name ++ "EditR"
               form = TH.varE $ TH.mkName formName
           entityId <- TH.varP $ TH.mkName nameId
           --  
           body <- [|  do 
               demo <- runDB $ get404 entityId  
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  (Just demo)
               defaultLayout $ do
                   let actionR = DemoEditR entityId       
                   $(widgetFile "Demo/DemoCreate") |]
           (:[]) <$> TH.funD method [return (TH.Clause [entityId] (TH.NormalB body) [])]
{-
listCrud :: String -> (TH.Q TH.Exp)
listCrud name =  do  
           let thName = TH.mkName $ "get" ++ name ++ "ListR"                      
           [|    do demos <- runDB $ selectList [] []                   
                    defaultLayout $ do
                       $(widgetFile "Demo/DemoList")|]                    

deleteCrud :: String -> (TH.Q TH.Exp)
deleteCrud name  =  do
          let thName = TH.mkName name            
          [| do runDB $ delete demoId
                redirect DemoListR |]


postEdit :: String -> (TH.Q TH.Exp)
postEdit name = do
           let thName = TH.mkName $ "post" ++ name ++ "EditR"
           [|  do 
                demo <- runDB $ get404 demoId
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  (Just demo)
                case result of
                     FormSuccess demoResult -> do 
                                 _ <- runDB $ replace demoId  demoResult
                                 redirect DemoListR
                     _ -> defaultLayout $ do     
                     let actionR = DemoEditR demoId                           
                     $(widgetFile "Demo/DemoCreate") |]
-}
