{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
module Routes (routes) where

import Control.Monad                  (msum, liftM, mzero)
import Control.Monad.Trans            (liftIO, lift)
import Happstack.Server               ( Browsing(EnableBrowsing), Method(GET), Response, ServerPart
                                      , decodeBody, defaultBodyPolicy, dir, methodM, serveDirectory
                                      , seeOther, toResponse)
import Happstack.Server.HSP.HTML      (defaultTemplate)
import Happstack.Auth.Core.Auth       (AskAuthState(..), getAuthId, AuthState)
import Happstack.Auth.Core.AuthURL    (AuthURL(..))
import Happstack.Auth.Core.Profile    (getUserId)
import Happstack.Auth.Core.ProfileURL (ProfileURL(P_PickProfile))
import Happstack.Auth.HSP.Login       (handleAuth)
import Happstack.Auth.HSP.AuthProfile (authProfileHandler)
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
              authProfileHandler "http://localhost:8000" "/foldr/" (authState state) (profileState state) defaultTemplate' Nothing Nothing "/"
              , requireAuth (authState state)
              , methodM GET >> seeOther "/foldr" (toResponse ())  -- redirect / to /foldr
              , dir "foldr" $ foldrDocument
              , serveDirectory EnableBrowsing ["index.html"] "public" -- static file serving
            ]

requireAuth :: AcidState AuthState -> App Response
requireAuth state =
    do ma <- getAuthId state
       case ma of
         Just _ -> mzero
         Nothing -> seeOther "/foldr/auth/login" (toResponse "")
