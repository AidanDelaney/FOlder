{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Routes (routes) where

import Control.Monad                  (msum, liftM, mzero)
import Control.Monad.Trans            (liftIO)
import Happstack.Server               ( Browsing(EnableBrowsing), Method(GET), Response, ServerPart
                                      , decodeBody, defaultBodyPolicy, dir, methodM, serveDirectory
                                      , seeOther, toResponse)
import Happstack.Server.HSP.HTML      (defaultTemplate)
import Happstack.Auth.Core.Auth       (AskAuthState(..))
import Happstack.Auth.Core.AuthURL    (AuthURL(..))
import Happstack.Auth.Core.Profile    (getUserId)
import Happstack.Auth.Core.ProfileURL (ProfileURL(P_PickProfile))
import Happstack.Auth.HSP.Login       (handleAuth)
import qualified HSX.XMLGenerator as HSX
import HSP 
import Data.Acid
import Web.Routes.Happstack           (implSite_)

import Pages                          (foldrDocument)
import State                          (App)
import Auth

routes :: AuthData -> App Response
routes state = 
    do decodeBody (defaultBodyPolicy "/tmp/" 4096 4096 4096)        -- decode the request body if present.
       msum [
             let baseURI = "http://localhost:8000" in -- FIXME: hardcoded hostname
             do r <- implSite_  baseURI "/foldr/" (spec state (Just baseURI))
                case r of
                 (Left e) -> liftIO (print e) >> mzero
                 (Right r) -> return r 
             , methodM GET >> seeOther "/foldr" (toResponse ())  -- redirect / to /foldr
             , dir "foldr" $ foldrDocument
             , serveDirectory EnableBrowsing ["index.html"] "public" -- static file serving
           ]