module Handler.Demo where

import Import
import Crud.Core


--Aform From Entity Demo
iglesiaForm :: Maybe Iglesia -> AForm Handler Iglesia
iglesiaForm   iglesia = Iglesia 
                <$> areq textField "nombre" (iglesiaNombre <$> iglesia)
                <*> areq textField "direccion" (iglesiaDireccion <$> iglesia) 
                <*> areq textField "telefono" (iglesiaTelefono <$> iglesia) 
                <*> areq textField "ciudad" (iglesiaCiudad <$> iglesia)

--                <*> areq textField "prueba" (iglesiaPrueba <$> iglesia)

--CRUD 
--Create
getNewRoute   "Iglesia" "iglesiaForm"
postNewRoute  "Iglesia" "iglesiaForm" "IglesiaListR"
getEditRoute  "Iglesia" "iglesiaForm"
postEditRoute "Iglesia" "iglesiaForm" "IglesiaListR"

deleteCrudRoute "Iglesia" "IglesiaListR"
listCrudRoute "Iglesia" "iglesiaNombre"


