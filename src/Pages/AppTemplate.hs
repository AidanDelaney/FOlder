{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.AppTemplate where

import Control.Applicative       ((<$>))
import HSP
import Happstack.Server          (ServerPart, ServerPartT, Response, toResponse)
import HSP.ServerPartT           () -- instance XMLGenerator (ServerPartT m)
import Happstack.Server.HSP.HTML ()
import State                     (App)

appTemplate ::
    ( EmbedAsChild (App) headers
    , EmbedAsChild (App) body
    ) =>
    String -> headers -> body -> App Response
appTemplate title headers body = toResponse <$> (unXMLGenT (appTemplate' title headers body))

appTemplate' :: 
    ( EmbedAsChild (App) headers
    , EmbedAsChild (App) body
    ) =>
    String -> headers -> body -> XMLGenT (App) XML
appTemplate' title headers body =
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title><% title %></title>

  <link rel="stylesheet" href="/theme/AlohaDocument.css" />

  <script type="text/javascript" src="/js/aloha/aloha.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Format/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Table/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.List/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Link/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/eu.phoric.aloha.plugins.Save/plugin.js"></script>

  <% headers %>
  </head>
  <body>
    <div id="main"> 
      <div id="bodyContent">
        <div id="content" class="article">
        <% body %>
        </div>
      </div>
    </div>
  <footer><p>To create a new document <a href="/foldr/new">Click here</a>.</p></footer>
  </body>
  </html>


