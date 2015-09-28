# Yesod-CRUD-Generator

the following project is an alternative to generate CRUD operation for the Framework Yesod. Actually is a beta.

this package generate four opeartion 

* create
* edit
* delete
* list 

###  Including the package within the project:

Currently the package is not found within the central repository so that to use it need to copy and paste the Crud folder in the root directory of your project

### Use

Yesod-CRUD generation features two forms of code generation:

**Generation methods "Route":** This is the easiest and quickest way to use the package it generated methods correspond to those defined in the routes file; however these do not allow the configuration of additional elements such as authentication, business logic among others. Then explains step by step how to use these methods

Entity: The first to be created in the state in the models file, then an example of an entity.

    Demo
        fieldOne Text 
        fieldTwo Int
        fieldThree Bool
        fieldFour  Day

