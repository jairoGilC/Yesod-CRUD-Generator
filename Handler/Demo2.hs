module Handler.Demo2 where

import Import
import Crud.Core



intencionForm :: Maybe Intencion -> AForm Handler Intencion
intencionForm   intencion = Intencion 
                <$> areq textField "intencion" (intencionIntencion <$> intencion)
                <*> areq textField "peticionario" (intencionPeticionario <$> intencion)
                <*> areq dayField "fecha" (intencionFecha <$> intencion)
                <*> areq (selectField iglesias) "Iglesia" (intencionIglesia <$> intencion)
          where iglesias :: Handler (OptionList IglesiaId)
                iglesias = do
                  entities <- runDB $ selectList [] [Asc IglesiaNombre]
                  optionsPairs $ iglesiaPair <$> entities
                iglesiaPair ct = (iglesiaNombre $ entityVal ct, entityKey ct)
                
getNewMethod   "Intencion" "intencionForm"
postNewMethod  "Intencion" "intencionForm" "IntencionListR"
getEditMethod  "Intencion" "intencionForm"
postEditMethod "Intencion" "intencionForm" "IntencionListR"

deleteCrudMethod "Intencion" "IntencionListR"
listCrudMethod "Intencion" "intencionPeticionario"

getIntencionNewR :: Handler Html 
getIntencionNewR  = do
                _ <- requireAuthId   
                getIntencionNew

postIntencionNewR :: Handler Html 
postIntencionNewR  = do 
                 _ <- requireAuthId 
                 postIntencionNew

getIntencionEditR :: IntencionId -> Handler Html 
getIntencionEditR  intencionid= do 
                        _ <- requireAuthId 
                        getIntencionEdit intencionid

postIntencionEditR :: IntencionId -> Handler Html 
postIntencionEditR  intencionid= do 
                         _ <- requireAuthId
                         postIntencionEdit intencionid

deleteIntencionDeleteR :: IntencionId -> Handler Html 
deleteIntencionDeleteR intencionid= do 
                            _ <- requireAuthId
                            deleteIntencionDelete intencionid

getIntencionListR :: Handler Html 
getIntencionListR = do 
                _ <- requireAuthId
                getIntencionList

