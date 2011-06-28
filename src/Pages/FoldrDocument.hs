{-# LANGUAGE FlexibleContexts, FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -F -pgmFtrhsx #-}
module Pages.FoldrDocument
     ( foldrDocument 
     ) where

import Control.Monad             (msum)
import Control.Monad.Reader      (ask)
import Data.Maybe                (isNothing)
import HSP
import Happstack.Server          (FromReqURI(..), Method(GET, PUT, POST, DELETE), 
                                  Response, ServerPart, ServerPartT, methodM, dir, 
                                  path, methodOnly, seeOther, toResponse)
import Data.Acid
import Data.Char                 (isSpace, digitToInt)
import HSP.ServerPartT           ()
import Happstack.Server.HSP.HTML ()
import Happstack.Data.IxSet      (getOne)
import Pages.AppTemplate         (appTemplate)
import State                     (App)
import State.Foldr               (DocId(..), GetDocument(..), AddDocument(..), GetNextDocId(..))
import Types.Foldr               (Foldr(..), Document(..))

foldrDocument :: App Response
foldrDocument =
       -- TODO: Need CRUD for Foldr Documents
       -- GET /      -> This users Foldr of Documents
       -- POST /     -> create a new doc
       -- GET /id    -> read doc of specific id
       -- PUT /id    -> edit doc of specific id
       -- DELETE /id -> delete doc of specific id
       -- /new       -> Form which POSTs content to /
       msum [getFoldr, getDocument, newDocument]

getFoldr :: App Response
getFoldr = 
    do methodM GET
       appTemplate "Folder" foldrEditableHeaders foldrDefaultBlurb

getDocument :: App Response
getDocument = 
    do methodOnly GET
       path $  getDocumentById

instance FromReqURI DocId where
    fromReqURI s = 
        case reads s of
          [(x, rest)] | all isSpace rest -> Just (DocId x)
          _         -> Nothing

query_  e = do store <- ask ; query' store e
update_ e = do store <- ask ; update' store e

getDocumentById :: DocId -> App Response
getDocumentById did =
    let document = query_ (GetDocument did) in
    do
      docset <- document
      appTemplate "Folder" foldrEditableHeaders ("doc:" ++ (show (getOne docset)))

newDocument :: App Response
newDocument =
    dir "new" $
       do methodM GET
          did   <- query_ GetNextDocId
          update_ (AddDocument (mkDoc did))
          seeOther ("/foldr/" ++ (show did)) (toResponse ())
          where
            mkDoc next = Document "Anonymous" next "Default title" "<p>Blank document</p>"

-- TODO: After template mangling, what exactly is the return type of this?
foldrEditableHeaders :: XMLGenT (App) XML
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
  </div>