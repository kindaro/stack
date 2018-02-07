module Experiment where

import Stack.Build.Source as S
import System.Directory as D
import System.Environment as E
import Data.Map (Map)
import Data.Set (Set)
import Control.Monad.IO.Class (MonadIO)

import RIO

import Stack.Types.PackageName (PackageName)
import Stack.Types.Config (lcLoadBuildConfig, defaultBuildOptsCLI)
import Stack.Types.Package (LocalPackage)
import Stack.Build.Target (Target, NeedTargets(..))
import Stack.Types.BuildPlan (LoadedSnapshot(..))
import Stack.Options.GlobalParser (globalOptsFromMonoid)
import Stack.Runners (loadConfigWithOpts)
import Stack.Setup (setupEnv)

import MainHelpers

obtainLoadConfig lc = lcLoadBuildConfig lc Nothing

data SourceMapFull = SourceMapFull
    { _packageMap :: Map PackageName Target
    , _loadedSnapshot :: LoadedSnapshot
    , _localPackages :: [LocalPackage]
    , _packageNames :: Set PackageName
    , _sourceMap :: SourceMap
    }

runme :: IO SourceMapFull
runme = do
    currentDir <- D.getCurrentDirectory
    progName <- E.getProgName

    (mon,run) <- withArgs ["build"] (commandLineHandler currentDir progName False)

    let opts = globalOptsFromMonoid False mon

    buildConfig <- loadConfigWithOpts opts obtainLoadConfig
    envConfig <- runRIO buildConfig (setupEnv Nothing)

        

    stuff <- runRIO envConfig (S.loadSourceMapFull NeedTargets defaultBuildOptsCLI)
    let ( packageMap , loadedSnapshot , localPackages , packageNames , sourceMap) = stuff
    return $ SourceMapFull packageMap loadedSnapshot localPackages packageNames sourceMap
