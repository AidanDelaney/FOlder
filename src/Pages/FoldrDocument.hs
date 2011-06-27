{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.FoldrDocument
     ( foldrDocument 
     ) where

import Control.Monad             (msum)
import Data.Maybe                (isNothing)
import HSP
import Happstack.Server          (FromReqURI(..), Method(GET, PUT, POST, DELETE), Response, ServerPart, ServerPartT, methodM, dir, path, ok)
import Data.Acid
import Data.Char                 (isSpace, digitToInt)
import HSP.ServerPartT           ()
import Happstack.Server.HSP.HTML ()
import Pages.AppTemplate         (appTemplate)
import State.Foldr               (DocId(..))

foldrDocument :: ServerPart Response
foldrDocument =
       -- TODO: Need CRUD for Foldr Documents
       -- GET /      -> This users Foldr of Documents
       -- POST /     -> create a new doc
       -- GET /id    -> read doc of specific id
       -- PUT /id    -> edit doc of specific id
       -- DELETE /id -> delete doc of specific id
       -- /new       -> Form which POSTs content to /
       msum [getDocument, getFoldr, newDocument]

--getFoldr :: ServerPartT IO (HSP XML)
getFoldr = 
    do methodM GET
       appTemplate "Folder" foldrEditableHeaders foldrDefaultBlurb


getDocument = 
    do methodOnly GET
       path $  getDocumentById

instance FromReqURI DocId where
    fromReqURI s = 
        case reads s of
          [(x, rest)] | all isSpace rest -> Just (DocId x)
          _         -> Nothing

getDocumentById :: DocId -> ServerPartT IO Response
getDocumentById did = appTemplate "Folder" foldrEditableHeaders ("id:" ++ (show did))

newDocument =
    dir "new" $
       do methodM GET
          appTemplate "Folder: New Document" foldrEditableHeaders "Type content here"

-- TODO: After template mangling, what exactly is the return type of this?
--foldrEditableHeaders :: ServerPartT IO (HSP XML)
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

foldrDefaultBlurb = 
  <div>
  <h1>Welcome to Folder</h1>
  <p>You can't save the changes to this document, but click on some of the text to see what Folder allows you to do.</p>
  <p>To create a new document <a href="/foldr/create">Click here</a>.</p>
  </div>