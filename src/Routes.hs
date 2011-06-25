{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Routes (routes) where

import Control.Monad    (msum)
import Happstack.Server ( Browsing(EnableBrowsing), Method(GET), Response, ServerPart
                        , decodeBody, defaultBodyPolicy, dir, methodM, serveDirectory
                        , seeOther, toResponse)
--import Happstack.Authenticate
import Pages            (foldrDocument)

routes :: ServerPart Response
routes = 
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)        -- decode the request body if present. 
       msum [ -- dir "login" $ withSession (\_ -> rootRedirect) defaultRoutes, 
              methodM GET >> seeOther "/foldr" (toResponse ())  -- redirect / to /foldr
              , dir "foldr" $ foldrDocument
              , serveDirectory EnableBrowsing ["index.html"] "public" -- static file serving
            ]

defaultRoutes :: ServerPart Response
defaultRoutes =
    msum [
         ]
