{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.FoldrDocument
     ( foldrDocument 
     ) where

import HSP
import Happstack.Server  (Method(GET), Response, ServerPart, ServerPartT, methodM)
import Happstack.State   (query)
import Pages.AppTemplate (appTemplate)

foldrDocument :: ServerPart Response
foldrDocument =
    do methodM GET
       appTemplate "Some Title" () "Hello World"