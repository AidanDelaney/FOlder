{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.AppTemplate where

import Control.Applicative ((<$>))
import HSP
import Happstack.Server    (ServerPart, ServerPartT, Response, toResponse)
import HSP.ServerPartT     () -- instance XMLGenerator (ServerPartT m)
import Happstack.Server.HSP.HTML ()

appTemplate ::
    ( EmbedAsChild (ServerPartT IO) headers
    , EmbedAsChild (ServerPartT IO) body
    ) =>
    String -> headers -> body -> ServerPart Response
appTemplate title headers body = toResponse <$> (unXMLGenT (appTemplate' title headers body))

appTemplate' :: 
    ( EmbedAsChild (ServerPartT IO) headers
    , EmbedAsChild (ServerPartT IO) body
    ) =>
    String -> headers -> body -> XMLGenT (ServerPartT IO) XML
appTemplate' title headers body =
  <html>
  <head>
  <meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
  <title><% title %></title>

  <script type="text/javascript" src="/js/aloha/aloha.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Format/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Table/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.List/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/com.gentics.aloha.plugins.Link/plugin.js"></script>
  <script type="text/javascript" src="/js/aloha/plugins/eu.phoric.aloha.plugins.Save/plugin.js"></script>


  <link rel="stylesheet" href="/theme/AlohaDocument.css" />

  <script type="text/javascript">
  GENTICS.Aloha.settings = {
	logLevels: {'error': true, 'warn': true, 'info': true, 'debug': true},
	errorhandling : false,
	ribbon: false,
	"i18n": {
		"acceptLanguage": 'en'
	},
	"plugins": {
	 	"com.gentics.aloha.plugins.Link": {
		  	// all links that match the targetregex will get set the target
 			// e.g. ^(?!.*aloha-editor.com).* matches all href except aloha-editor.com
		  	targetregex : '^(?!.*aloha-editor.com).*',
		  	// this target is set when either targetregex matches or not set
		    // e.g. _blank opens all links in new window
		  	target : '_blank',
		  	// the same for css class as for target
		  	cssclassregex : '^(?!.*aloha-editor.com).*',
		  	cssclass : 'external'
		},
	 	"com.gentics.aloha.plugins.Table": {
			config: ['table']
		}
  	}
  };

  $(document).ready(function() {
	$('#content').aloha();
  });  
  </script>
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
  </body>
  </html>

