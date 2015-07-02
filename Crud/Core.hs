{-# LANGUAGE DeriveDataTypeable #-}

module Crud.Core where

-- import Data.Generics
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Data.Data
import Import
import Yesod.Form.Bootstrap3



demoForm :: Maybe Demo -> AForm Handler Demo
demoForm   demo = Demo 
                <$> areq textField "fieldone" (demoFieldOne <$> demo)
                <*> areq intField "fieldTwo" (demoFieldTwo <$> demo) 
                <*> areq boolField "fieldThree" (demoFieldThree <$> demo) 
                <*> areq dayField "fieldFour" (demoFieldFour <$> demo) 

demoId = "demoId"

getNew :: String -> String -> TH.Q TH.Dec
getNew name formName = do
           let method = TH.mkName $ "get" ++ name ++ "NewR"
               form = TH.mkName $ formName
               formV = return (TH.VarE formName)
           body <- [| do 
             (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $formV Nothing
             defaultLayout $ do
                let actionR = DemoNewR                          
                $(widgetFile "Demo/DemoCreate") |]
           TH.funD method [return (TH.Clause [] (TH.NormalB body) [])]

{-
postNew :: String -> (TH.Q TH.Exp)
postNew name = do
           let thName = TH.mkName $ "post" ++ name ++ "NewR"            
           [| do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  Nothing
                case result of
                     FormSuccess demo -> do 
                                 _ <- runDB $ insert demo
                                 redirect DemoListR
                     _ -> defaultLayout $ do
                     let actionR = DemoNewR                
                     $(widgetFile "Demo/DemoCreate")|]  

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

getEdit :: String -> (TH.Q TH.Exp)
getEdit name = do
           let thName = TH.mkName $ "get" ++ name ++ "EditR"
           [|  do 
               demo <- runDB $ get404 demoId  
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  (Just demo)
               defaultLayout $ do
                   let actionR = DemoEditR demoId       
                   $(widgetFile "Demo/DemoCreate") |]

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


getNew'      name =  TH.runQ $ getNew name
postNew'     name =  TH.runQ $ postNew name
getEdit'     name =  TH.runQ $ getEdit name
postEdit'    name =  TH.runQ $ postEdit name
deleteCrud'  name =  TH.runQ $ deleteCrud name 
listCrud'    name =  TH.runQ $ listCrud name

-}