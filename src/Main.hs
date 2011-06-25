module Main where

import Data.Version (showVersion)

import Control.Concurrent (MVar, forkIO, killThread)
import Control.Exception (bracket)

import System.Environment (getArgs)
import System.Log.Logger (Priority(..), logM)
import System.Exit (exitWith, ExitCode(..), exitFailure)
import System.Console.GetOpt 

import Happstack.Util.Cron (cron)
import Happstack.State (waitForTermination)
import Happstack.Server
  ( Conf(port)
  , simpleHTTP
  , nullConf
  , validator
  , wdgHTMLValidator
  )
import Happstack.State
  ( Component
  , Proxy(..)
  , Methods
  , TxControl
  , Saver(Queue, FileSaver)
  , runTxSystem
  , shutdownSystem
  , createCheckpoint
  )

import Logger (withLogger)
import Routes (routes)
import State  (AppState)

-- NOTE: You need to change this file name such that it reflects the name of
-- your project in the .cabal file, because it is auto-generated by cabal.
import Paths_foldr (version)


------------------------------------------------------------------------------
-- Main Function
------------------------------------------------------------------------------

main = withLogger $ do

  -- convert command line arguments into list of flags
  flags <- parseConfig =<< getArgs

  -- display information like help or version, if required
  displayInfo flags

  -- run server
  runServer flags


-- | Display information required by flags. Currently, this is either the help
-- message or the version information.
displayInfo :: [Flag] -> IO ()
displayInfo flags
  | any isHelp    flags = putStr helpMessage >> exitWith ExitSuccess
  | any isVersion flags = putStr versionInfo >> exitWith ExitSuccess
  | otherwise           = return ()


------------------------------------------------------------------------------
-- Command Line Interface Content
------------------------------------------------------------------------------

-- | Program name used to identify your application. 
-- It influences the version and help message, as well as the directory name
-- for the state of your application.
progName = "foldr-server"

fullName = "Organise documents in a foldr"

copyrightInfo = "Copyright (C) 2011 Aidan Delaney"
licenceInfo = "AGPLv3: See the accompanying licence for copying conditions."

-- | Version information extracted from Paths_guestbook
versionInfo = unlines
  [ fullName ++ " (" ++ progName ++ "), version " ++ showVersion version
  , copyrightInfo
  , licenceInfo
  ]

-- | A simple usage message listing all flags possible.
helpMessage = usageInfo header opts
  where 
  header = "Usage: "++progName++" [OPTION...]"


------------------------------------------------------------------------------
-- Server
------------------------------------------------------------------------------

-- | Configuration information
data AppConf
    = AppConf { httpConf :: Conf
              , store :: FilePath
              , static :: FilePath 
              }

-- | Default configuration
-- FIXME: Should incorporate directories queried via Paths_guestbook module
defaultConf :: String -> AppConf
defaultConf progName
    = AppConf { httpConf = nullConf
              , store    = "_local/" ++ progName ++ "_state"
              , static   = "public"
              }

-- | Run the server with the given configuration.
runServer :: [Flag] -> IO ()
runServer flags = do
  -- combine all server config flags
  let appConf = foldr ($) (defaultConf progName) [f | ServerConfig f <- flags] 

  -- start the state system
  withSystemState' (store appConf) stateProxy $ \control -> do
    
     -- start the http server
     withThread (simpleHTTP (httpConf appConf) routes) $ do

       -- checkpoint the state once a day
       withThread (cron (60*60*24) (createCheckpoint control)) $ do
  
          -- wait for termination signal
          logM "Happstack.Server" NOTICE "System running, press 'e <ENTER>' or Ctrl-C to stop server" 
          waitForTermination

  where
  startSystemState' :: (Component st, Methods st) => String -> Proxy st -> IO (MVar TxControl)
  startSystemState' = runTxSystem . Queue . FileSaver

  withSystemState' :: (Component st, Methods st) => String -> Proxy st -> (MVar TxControl -> IO a) -> IO a
  withSystemState' name proxy action = 
    bracket (startSystemState' name proxy) createCheckpointAndShutdown action

  withThread init action = bracket (forkIO $ init) (killThread) (\_ -> action)

  createCheckpointAndShutdown control = do
    logM "Happstack.Server" NOTICE "Creating system checkpoint" 
    createCheckpoint control
    logM "Happstack.Server" NOTICE "System shutdown" 
    shutdownSystem control

  -- simon.meier@inf.ethz.ch: I currently don't know what this is used for!
  -- FIXME: Add sensible comment.
  stateProxy :: Proxy AppState
  stateProxy = Proxy


------------------------------------------------------------------------------
-- Command Line Parsing
------------------------------------------------------------------------------

-- | Flags
data Flag = 
    ServerConfig (AppConf -> AppConf)
  | Help
  | Version

-- Flag selectors
isHelp    flag = case flag of Help    -> True; _ -> False
isVersion flag = case flag of Version -> True; _ -> False

-- | Command line options.
opts :: [OptDescr Flag]
opts = [ Option [] ["http-port"]   (ReqArg setPort "port")                        "Port to bind http server"
       , Option [] ["no-validate"] (NoArg $ setValidator Nothing)                 "Turn off HTML validation"
       , Option [] ["validate"]    (NoArg $ setValidator (Just wdgHTMLValidator)) "Turn on HTML validation"
       , Option [] ["store"]       (ReqArg setMacidDir "PATH")                    "The directory used for database storage."
       , Option [] ["static"]      (ReqArg setStaticDir "PATH")                   "The directory searched for static files" 
       , Option [] ["version"]     (NoArg Version)                                "Display version information" 
       , Option [] ["help"]        (NoArg Help)                                   "Display this help message" 
       ]
     where
     setPort p      = ServerConfig $ \c -> c { httpConf = (httpConf c) {port      = read p} }
     setValidator v = ServerConfig $ \c -> c { httpConf = (httpConf c) {validator = v     } }
     setMacidDir p  = ServerConfig $ \c -> c { store = p }
     setStaticDir p = ServerConfig $ \c -> c { static = p }

-- | Parse the command line arguments into a list of flags. Exits with usage
-- message, in case of failure.
parseConfig :: [String] -> IO [Flag]
parseConfig args = case getOpt Permute opts args of
  (flags,_,[]) -> return flags
  (_,_,errs)   -> do
    logM progName ERROR ("Failure while parsing command line:\n"++unlines errs)
    putStr helpMessage
    exitFailure
