{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances, TemplateHaskell, MultiParamTypeClasses, RecordWildCards, TypeFamilies #-}

module Auth where

import Control.Exception           (bracket)
import Control.Monad               (msum, liftM, mzero)
import Control.Monad.Trans         (liftIO)
import Control.Monad.Reader
import Control.Monad.State
import Data.Acid
import Data.Maybe                  (fromMaybe)
import Data.SafeCopy
import Data.Generics
import HSP
import qualified HSX.XMLGenerator as HSX
import Happstack.Server
import Happstack.Auth.Core.Auth
import Happstack.Auth.Core.AuthURL
import Happstack.Auth.Core.Profile
import Happstack.Auth.Core.ProfileURL
import Happstack.Auth.HSP.Login    (handleAuth, handleProfile)
import Happstack.Server.HSP.HTML   (defaultTemplate)
import Happstack.Data.IxSet        (IxSet, (@=), getOne, inferIxSet, noCalcs)
import qualified Happstack.Data.IxSet  as IxSet
import System.FilePath             ((</>))
import Web.Routes
import Web.Routes.TH
import Data.Text                   (Text)
import qualified Data.Text         as Text

import Pages.FoldrDocument
import State

data ProfileData = 
    ProfileData { dataFor :: UserId
                , profileMsg :: Text
                }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileData)

$(inferIxSet "ProfilesData" ''ProfileData 'noCalcs [''UserId, ''Text])

data ProfileDataState =
    ProfileDataState { profilesData :: ProfilesData }
    deriving (Eq, Ord, Read, Show, Typeable, Data)
$(deriveSafeCopy 1 'base ''ProfileDataState)

newProfileData :: UserId -> Text -> Update ProfileDataState ProfileData
newProfileData uid msg =
    do pds@(ProfileDataState {..}) <- get       
       let profileData = ProfileData uid msg
       put $ pds { profilesData = IxSet.insert profileData profilesData }
       return profileData

askProfileData :: UserId -> Query ProfileDataState (Maybe ProfileData)
askProfileData uid =
    do ProfileDataState{..} <- ask
       return $ getOne $ profilesData @= uid

$(makeAcidic ''ProfileDataState 
                [ 'newProfileData
                , 'askProfileData
                ]
 )
 
initialProfileDataState :: ProfileDataState
initialProfileDataState = ProfileDataState { profilesData = IxSet.empty }

data ProfileDataURL 
    = CreateNewProfileData 
    | ViewProfileData UserId
      deriving (Eq, Ord, Read, Show, Data, Typeable)

$(derivePathInfo ''ProfileDataURL)

handleProfileData authStateH profileStateH profileDataStateH url =
    case url of
      CreateNewProfileData ->
          do mUserId <- getUserId authStateH profileStateH
             case mUserId of
               Nothing -> internalServerError $ toResponse $ "not logged in."
               (Just userId) ->
                   do update' profileDataStateH (NewProfileData userId (Text.pack "this is the default message."))
                      seeOther "/" (toResponse "/")
      (ViewProfileData uid) ->
          do mProfileData <- query' profileDataStateH (AskProfileData uid)
             ok $ toResponse $ show mProfileData

data SiteURL
    = U_HomePage
    | U_Auth AuthURL
    | U_Profile ProfileURL
    | U_ProfileData ProfileDataURL
    deriving (Eq, Ord, Read, Show)

$(derivePathInfo ''SiteURL)

data AuthData = AuthData 
  { authState        :: AcidState AuthState
  , profileState     :: AcidState ProfileState
  , profileDataState :: AcidState ProfileDataState
  }

withAuth :: (AuthData -> IO a) -> IO a
withAuth f =
  let basePath = "_state" in
  bracket (openAcidStateFrom (basePath </> "auth")        initialAuthState)        (createCheckpointAndClose) $ \auth ->
  bracket (openAcidStateFrom (basePath </> "profile")     initialProfileState)     (createCheckpointAndClose) $ \profile ->
  bracket (openAcidStateFrom (basePath </> "profileData") initialProfileDataState) (createCheckpointAndClose) $ \profileData ->
    f (AuthData auth profile profileData)

defaultTemplate' :: (XMLGenerator m, EmbedAsChild m h, EmbedAsChild m b, HSX.XML m ~ XML) => String -> h -> b -> m Response
defaultTemplate' t h b = liftM toResponse (defaultTemplate t h b)

spec :: AuthData -> Maybe String -> Site SiteURL (App Response)
spec acid realm =
    setDefault U_HomePage $
      Site { handleSite          = \f u -> unRouteT (handle acid realm u) f
           , formatPathSegments  = \u -> (toPathSegments u, [])
           , parsePathSegments   = parseSegments fromPathSegments
           }

handle :: AuthData -> Maybe String -> SiteURL -> RouteT SiteURL (App) Response
handle auth@AuthData{..} realm url =
    case url of
      U_HomePage          -> fooDocument auth
      (U_Auth auth)       -> do onAuthURL <- showURL (U_Profile P_PickProfile)
                                nestURL U_Auth $ handleAuth authState defaultTemplate' realm onAuthURL auth
      (U_Profile profile) -> do postPickedURL <- showURL (U_ProfileData CreateNewProfileData)
                                nestURL U_Profile $ handleProfile authState profileState defaultTemplate' postPickedURL profile
      (U_ProfileData profileDataURL) ->
                             do handleProfileData authState profileState profileDataState profileDataURL


fooDocument :: AuthData ->  RouteT SiteURL (App) Response
fooDocument AuthData{..} = lift $ foldrDocument