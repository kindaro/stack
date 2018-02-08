module Experiment where

import Stack.Build.Source as S
import System.Directory as D
import System.Environment as E
import Data.Map (Map)
import Data.Set (Set)

import RIO

import Stack.Types.PackageName (PackageName)
import Stack.Types.Config (lcLoadBuildConfig, defaultBuildOptsCLI, LoadConfig, BuildConfig, EnvConfig)
import Stack.Types.Package (LocalPackage)
import Stack.Build.Target (Target, NeedTargets(..))
import Stack.Types.BuildPlan (LoadedSnapshot(..))
import Stack.Options.GlobalParser (globalOptsFromMonoid)
import Stack.Runners (loadConfigWithOpts)
import Stack.Setup (setupEnv)

import MainHelpers

obtainBuildConfig :: LoadConfig -> IO BuildConfig
obtainBuildConfig lc = lcLoadBuildConfig lc Nothing

data SourceMapFull = SourceMapFull
    { _packageMap :: Map PackageName Target
    , _loadedSnapshot :: LoadedSnapshot
    , _localPackages :: [LocalPackage]
    , _packageNames :: Set PackageName
    , _sourceMap :: SourceMap
    }

obtainEnvConfig :: IO EnvConfig
obtainEnvConfig = do
    currentDir <- D.getCurrentDirectory
    progName <- E.getProgName

    (mon,_) <- withArgs ["build"] (commandLineHandler currentDir progName False)

    let opts = globalOptsFromMonoid False mon

    buildConfig <- loadConfigWithOpts opts obtainBuildConfig
    envConfig <- runRIO buildConfig (setupEnv Nothing)

    return envConfig

obtainSourceMapFull :: EnvConfig -> IO SourceMapFull
obtainSourceMapFull envConfig = do
    stuff <- runRIO envConfig (S.loadSourceMapFull NeedTargets defaultBuildOptsCLI)
    let ( packageMap , loadedSnapshot , localPackages , packageNames , sourceMap) = stuff
    return $ SourceMapFull packageMap loadedSnapshot localPackages packageNames sourceMap
