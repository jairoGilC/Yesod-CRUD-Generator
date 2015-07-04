module Handler.Demo where

import Import
import Crud.Core
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3,
                              )


--Aform From Entity Demo
demoForm :: Maybe Demo -> AForm Handler Demo
demoForm   demo = Demo 
                <$> areq textField "fieldone" (demoFieldOne <$> demo)
                <*> areq intField "fieldTwo" (demoFieldTwo <$> demo) 
                <*> areq boolField "fieldThree" (demoFieldThree <$> demo) 
                <*> areq dayField "fieldFour" (demoFieldFour <$> demo) 
--AForm demo 2
demo2Form :: Maybe Demo2 -> AForm Handler Demo2
demo2Form   demo = Demo2 
                <$> areq textField "fieldone" (demo2FieldOne <$> demo)

--CRUD 
--Create
getNew "Demo" "demoForm"
postNew "Demo" "demoForm"
-- getEdit "Demo" "DemoId" "demoForm"

{-
getDemoNewR ::  Handler Html 
getDemoNewR = do 
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm Nothing
               defaultLayout $ do
                    let actionR = DemoNewR                          
                    $(widgetFile "Demo/DemoCreate") 


postDemoNewR :: Handler Html
postDemoNewR = do
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  Nothing
                case result of
                     FormSuccess demo -> do 
                                 _ <- runDB $ insert demo
                                 redirect DemoListR
                     _ -> defaultLayout $ do
                     let actionR = DemoNewR                
                     $(widgetFile "Demo/DemoCreate")



--Edit
getDemoEditR :: DemoId -> Handler Html
getDemoEditR demoId  = do
               demo <- runDB $ get404 demoId  
               (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  (Just demo)
               defaultLayout $ do
                   let actionR = DemoEditR demoId       
                   $(widgetFile "Demo/DemoCreate")
-}

postDemoEditR :: DemoId -> Handler Html
postDemoEditR demoId  = do
                demo <- runDB $ get404 demoId
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ demoForm  (Just demo)
                case result of
                     FormSuccess demoResult -> do 
                                 _ <- runDB $ replace demoId  demoResult
                                 redirect DemoListR
                     _ -> defaultLayout $ do     
                     let actionR = DemoEditR demoId                           
                     $(widgetFile "Demo/DemoCreate") 

--Delete
deleteDemoDeleteR ::  DemoId -> Handler Html
deleteDemoDeleteR demoId = do
                            runDB $ delete demoId
                            redirect DemoListR

--List
getDemoListR ::  Handler Html
getDemoListR  = do
                    demos <- runDB $ selectList [] []                   
                    defaultLayout $ do
                       $(widgetFile "Demo/DemoList")


{-
resultsForPage pageNumber = do
    let resultsPerPage = 10
    selectList
        []
        [ OffsetBy $ (pageNumber - 1) * resultsPerPage ]-}
