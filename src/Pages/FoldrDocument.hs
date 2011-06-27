{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.FoldrDocument
     ( foldrDocument 
     ) where

import HSP
import Happstack.Server          (Method(GET), Response, ServerPart, ServerPartT, methodM)
import Data.Acid
import HSP.ServerPartT           ()
import Happstack.Server.HSP.HTML ()
import Pages.AppTemplate         (appTemplate)
import State.Foldr

foldrDocument :: ServerPart Response
foldrDocument =
    do methodM GET
       -- TODO: Need CRUD for Foldr Documents
       -- /          -> This users Foldr of Documents
       -- /create    -> create a new doc
       -- /id        -> read doc of specific id
       -- /id/edit   -> edit doc of specific id
       -- /id/delete -> delete doc of specific id
       appTemplate "Some Title" foldrEditableHeaders "Hello World"

-- TODO: After template mangling, what exactly is the return type of this?
--foldrEditableHeaders :: ServerPartT IO
foldrEditableHeaders = 
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