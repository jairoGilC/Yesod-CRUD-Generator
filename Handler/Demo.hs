module Handler.Demo where

import Import
import Crud.Core



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
getNew   "Demo" "demoForm"
postNew  "Demo" "demoForm" "DemoListR"
getEdit  "Demo" "demoForm"
postEdit "Demo" "demoForm" "DemoListR"

deleteCrud "Demo" "DemoListR"
listCrud "Demo" "demoFieldOne"

getNew   "Demo2" "demo2Form"
postNew  "Demo2" "demo2Form" "Demo2ListR"
getEdit  "Demo2" "demo2Form"
postEdit "Demo2" "demo2Form" "Demo2ListR"

deleteCrud "Demo2" "Demo2ListR"
listCrud "Demo2" "demo2FieldOne"