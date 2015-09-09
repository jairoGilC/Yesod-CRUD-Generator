{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE RankNTypes #-}

module Crud.Core where

-- import Data.Generics
import qualified Language.Haskell.TH as TH
-- import Language.Haskell.TH.Quote

-- import Data.Data
import Import
import Yesod.Form.Bootstrap3
import Text.Blaze

{-
Este conjunto de operaciones permite al programador realizar 
las operaciones de crear  
-}
getNewRoute :: String -> String -> TH.Q [TH.Dec]
getNewRoute name formName = do
  let method = TH.mkName $ "get" ++ name ++ "NewR"
  getNew name formName method

getNewMethod :: String -> String -> TH.Q [TH.Dec]
getNewMethod name formName = do
  let method = TH.mkName $ "get" ++ name ++ "New"
  getNew name formName method

postNewRoute :: String -> String -> String  -> TH.Q [TH.Dec]
postNewRoute name formName redirectName = do
  let method = TH.mkName $ "post" ++ name ++ "NewR"
  postNew name formName redirectName method

postNewMethod :: String -> String -> String  -> TH.Q [TH.Dec]
postNewMethod name formName redirectName = do
  let method = TH.mkName $ "post" ++ name ++ "New"
  postNew name formName redirectName method

{-
Este conjunto de operaciones permite al programador realizar 
las operaciones de Editar  
-}
getEditRoute :: String -> String -> TH.Q [TH.Dec]
getEditRoute name formName = do
  let method = TH.mkName $ "get" ++ name ++ "EditR"
  getEdit name formName method

getEditMethod :: String -> String -> TH.Q [TH.Dec]
getEditMethod name formName = do
  let method = TH.mkName $ "get" ++ name ++ "Edit"
  getEdit name formName method

postEditRoute :: String -> String -> String  -> TH.Q [TH.Dec]
postEditRoute name formName redirectName = do
  let method = TH.mkName $ "post" ++ name ++ "EditR"
  postEdit name formName redirectName method

postEditMethod :: String -> String -> String  -> TH.Q [TH.Dec]
postEditMethod name formName redirectName = do
  let method = TH.mkName $ "post" ++ name ++ "Edit"
  postEdit name formName redirectName method  

{-
Este conjunto de operaciones permite al programador realizar 
las operaciones de eliminar  
-}
deleteCrudRoute :: String -> String -> TH.Q [TH.Dec]
deleteCrudRoute name  redirectName = do
  let method = TH.mkName $ "delete" ++ name ++ "DeleteR"
  deleteCrud name redirectName method

deleteCrudMethod :: String -> String -> TH.Q [TH.Dec]
deleteCrudMethod name  redirectName = do
  let method = TH.mkName $ "delete" ++ name ++ "Delete"
  deleteCrud name  redirectName method

{-
Este conjunto de operaciones permite al programador realizar 
las operaciones de listar  
-}
listCrudRoute :: String -> String -> TH.Q [TH.Dec]
listCrudRoute name fieldListName = do
  let method = TH.mkName $ "get" ++ name ++ "ListR"     
  listCrud name fieldListName method

listCrudMethod :: String -> String -> TH.Q [TH.Dec]
listCrudMethod name fieldListName = do
  let method = TH.mkName $ "get" ++ name ++ "List"     
  listCrud name fieldListName method




{-
Este conjunto de operaciones desarrolla la logica de los hamlet
-}
getNew :: String -> String -> TH.Name ->  TH.Q [TH.Dec]
getNew name formName method = do
  let form = TH.varE $ TH.mkName formName
      action = TH.conE $ TH.mkName $ name ++ "NewR"
      body = [| do
                 (widget, encoding) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ $form Nothing
                 defaultLayout $ do
                    let actionR = $action                          
                    formHamlet widget encoding actionR |]
  typ <- TH.sigD method [t|  HandlerT App IO Html |]
  fun <- TH.funD method [TH.clause [] (TH.normalB body) []]
  return [typ,fun]


postNew :: String -> String -> String -> TH.Name ->  TH.Q [TH.Dec]
postNew name formName redirectName method = do
           let form = TH.varE $ TH.mkName formName            
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

getEdit :: String -> String -> TH.Name -> TH.Q [TH.Dec]
getEdit name formName method = do
  entityName <- TH.newName "eId"
  let form = TH.varE $ TH.mkName formName
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

postEdit :: String -> String -> String ->  TH.Name -> TH.Q [TH.Dec]
postEdit name formName redirectName method= do
  entityName <- TH.newName "eId"
  let form = TH.varE $ TH.mkName formName
      action = TH.conE $ TH.mkName $ name ++ "EditR"
      entityId = TH.varE entityName
      entityParam = TH.varP entityName
      entityType  = TH.conT $ TH.mkName $ name ++ "Id"
      redirectAction = TH.conE $ TH.mkName redirectName      
      body =    [|  do 
                entity <- runDB $ get404 $entityId
                ((result,widget), encoding) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ $form  (Just entity)
                case result of
                     FormSuccess resultForm -> do 
                                 _ <- runDB $ replace $entityId  resultForm
                                 redirect $redirectAction
                     _ -> defaultLayout $ do     
                     let actionR = $action $entityId                          
                     formHamlet widget encoding actionR |]
  typ <- TH.sigD method [t| $entityType ->  HandlerT App IO Html |]         
  fun <- TH.funD method [TH.clause [entityParam] (TH.normalB body) []]
  return [typ,fun]

deleteCrud :: String  -> String -> TH.Name ->  TH.Q [TH.Dec]
deleteCrud name  redirectName method=  do
  entityName <- TH.newName "eId"
  let entityId = TH.varE entityName
      entityParam = TH.varP entityName
      entityType  = TH.conT $ TH.mkName $ name ++ "Id"
      redirectAction = TH.conE $ TH.mkName redirectName      
      body = [| do 
             runDB $ delete $entityId
             redirect $redirectAction |]
  typ <- TH.sigD method [t| $entityType ->  HandlerT App IO Html |] 
  fun <- TH.funD method [TH.clause [entityParam] (TH.normalB body) []]
  return [typ,fun]

listCrud :: String  -> String -> TH.Name -> TH.Q [TH.Dec]
listCrud name fieldListName method=  do 
  let fieldList = TH.varE $ TH.mkName fieldListName
      newAction = TH.conE $ TH.mkName (name ++ "NewR") 
      editAction = TH.conE $ TH.mkName (name ++ "EditR")  
      deleteAction = TH.conE $ TH.mkName (name ++ "DeleteR")  
      body = [| do  list <- runDB $ selectList [] []                   
                    defaultLayout $ do
                       listHamlet list  $fieldList $newAction $editAction $deleteAction
                       toWidgetBody deleteJulius|]  
  typ <- TH.sigD method [t|  HandlerT App IO Html |]                                         
  fun <- TH.funD method [TH.clause [] (TH.normalB body) []]
  return [typ,fun]

{-
Este conjunto de operaciones general lo interfaz para crear editar y listar
y el javascript para eliminar
-}

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

listHamlet :: forall site (m :: * -> *) (t :: * -> *) t1 a.
              (ToMarkup a, MonadThrow m, MonadBaseControl IO m,
              MonoFoldable (t (Entity t1)), MonadIO m, Foldable t) =>
              t (Entity t1)
              -> (t1 -> a)
              -> Route site
              -> (Key t1 -> Route site)
              -> (Key t1 -> Route site)
              -> WidgetT site m ()
listHamlet list identificateAtribute new edit deleteRoute= [whamlet|
<div>

<h1> DATA
$if null list
    <p> There are no data 
$else
    <table .table .table-responsive .table-hover>
        <thead>
                     <th> identificate                 
                     <th> edit
                     <th> delete
          
        $forall Entity entityId entity <- list
                        
            <tbody>
                <tr>
                    <td> 
                        #{identificateAtribute entity}
                    
                   <td >
                        <button  onclick=deletePost('@{deleteRoute entityId}') .btn .btn-danger>
                              <span .glyphicon .glyphicon-trash>
                            delete 

                   <td>
                       <a href=@{edit entityId} .btn .btn-warning .pull-right> 
                          <span .glyphicon .glyphicon-edit>
                          edit

<a  href=@{new} .btn .btn-primary .pull-right>
                              <span .glyphicon .glyphicon-plus>
                              create
|]

deleteJulius :: forall url. JavascriptUrl url
deleteJulius  = [julius|
function deletePost(deleteUrl) {
        $.ajax({ 
                url: deleteUrl, 
                type: 'DELETE', 
                success: function(result) { 
                location.reload(); 
                 } 
        }); 
}
|]

-- formParam :: [String] ->  [(TH.Exp, TH.Pat,TH.Type)]
{- formParam []      = []
formParam (x:[])  = do 
      entityName <- TH.newName $ "e" ++ x 
      let  entityId = TH.varE entityName
           entityParam = TH.varP entityName
           entityType  = TH.conT $ TH.mkName $ x 
      [(entityId,entityParam,entityType)]
                     
formParam (x:xs)  = []
-}