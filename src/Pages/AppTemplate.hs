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

  <link rel="stylesheet" href="/theme/FoldrDocument.css" />
  <link rel="stylesheet" href="/theme/AlohaDocument.css" />

  <script type="text/javascript" src="/js/aloha/aloha.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Format/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Table/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.List/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Link/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/eu.phoric.aloha.plugins.Save/plugin.js"></script>
  <script type="text/javascript">
  foldr_save=function(){
    var content= encodeURIComponent($('div#foldr').html());
    var title  = encodeURIComponent($('h1').html())
    $.ajax({
	type : "POST",
	url  : "",
        data : 'content=' + content + '&title=' + title,
	//success: function(msg){}; // do nothing
    });
  }
  </script>

  <% headers %>
  </head>
  <body>
    <header><ul><li> <a href="/foldr/new">New</a></li><li><a href="#" onClick="foldr_save();return false;">Save</a></li></ul></header>
    <div id="main"> 
      <div id="bodyContent">
        <div id="content" class="article">
        <% body %>
        </div>
      </div>
    </div>
  <footer><p>FOlder, (c) Aidan Delaney 2011, <a href="http://darcsden.com/AidanDelaney/FOlder">Available under the AGPL</a>; <a href="https://flattr.com/thing/331345/FOlder">Donate</a>.</p></footer>
  </body>
  </html>


