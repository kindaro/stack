{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Stack.Script
    ( scriptCmd
    ) where

import           Stack.Prelude
import qualified Data.ByteString.Char8      as S8
import qualified Data.Conduit.List          as CL
import           Data.List.Split            (splitWhen)
import qualified Data.Set                   as Set
import qualified Data.Text                  as T
import           Path
import           Path.IO
import qualified Stack.Build
import           Stack.Exec
import           Stack.GhcPkg               (ghcPkgExeName)
import           Stack.Options.ScriptParser
import           Stack.Runners
import           Stack.Types.Compiler
import           Stack.Types.Config
import           Stack.Types.PackageName
import           Stack.ImportParser
import           System.FilePath            (dropExtension, replaceExtension)
import           System.Process.Read

-- | Run a Stack Script
scriptCmd :: ScriptOpts -> GlobalOpts -> IO ()
scriptCmd opts go' = do
    file <- resolveFile' $ soFile opts
    let go = go'
            { globalConfigMonoid = (globalConfigMonoid go')
                { configMonoidInstallGHC = First $ Just True
                }
            , globalStackYaml = SYLNoConfig $ parent file
            }
    withBuildConfigAndLock go $ \lk -> do
        -- Some warnings in case the user somehow tries to set a
        -- stack.yaml location. Note that in this functions we use
        -- logError instead of logWarn because, when using the
        -- interpreter mode, only error messages are shown. See:
        -- https://github.com/commercialhaskell/stack/issues/3007
        case globalStackYaml go' of
          SYLOverride fp -> logError $ T.pack
            $ "Ignoring override stack.yaml file for script command: " ++ fp
          SYLDefault -> return ()
          SYLNoConfig _ -> assert False (return ())

        config <- view configL
        menv <- liftIO $ configEnvOverride config defaultEnvSettings
        wc <- view $ actualCompilerVersionL.whichCompilerL
        colorFlag <- appropriateGhcColorFlag

        targetsSet <-
            case soPackages opts of
                [] -> getParsedImports (soFile opts)
                packages -> do
                    let targets = concatMap wordsComma packages
                    targets' <- mapM parsePackageNameFromString targets
                    return $ Set.fromList targets'

        unless (Set.null targetsSet) $ do
            -- Optimization: use the relatively cheap ghc-pkg list
            -- --simple-output to check which packages are installed
            -- already. If all needed packages are available, we can
            -- skip the (rather expensive) build call below.
            bss <- sinkProcessStdout
                Nothing menv (ghcPkgExeName wc)
                ["list", "--simple-output"] CL.consume -- FIXME use the package info from envConfigPackages, or is that crazy?
            let installed = Set.fromList
                          $ map toPackageName
                          $ words
                          $ S8.unpack
                          $ S8.concat bss
            if Set.null $ Set.difference (Set.map packageNameString targetsSet) installed
                then logDebug "All packages already installed"
                else do
                    logDebug "Missing packages, performing installation"
                    Stack.Build.build (const $ return ()) lk defaultBuildOptsCLI
                        { boptsCLITargets = map packageNameText $ Set.toList targetsSet
                        }

        let ghcArgs = concat
                [ ["-hide-all-packages"]
                , maybeToList colorFlag
                , map (\x -> "-package" ++ x)
                    $ Set.toList
                    $ Set.insert "base"
                    $ Set.map packageNameString targetsSet
                , case soCompile opts of
                    SEInterpret -> []
                    SECompile -> []
                    SEOptimize -> ["-O2"]
                ]
        munlockFile lk -- Unlock before transferring control away.
        case soCompile opts of
          SEInterpret -> exec menv ("run" ++ compilerExeName wc)
                (ghcArgs ++ toFilePath file : soArgs opts)
          _ -> do
            let dir = parent file
            -- use sinkProcessStdout to ensure a ProcessFailed
            -- exception is generated for better error messages
            sinkProcessStdout
              (Just dir)
              menv
              (compilerExeName wc)
              (ghcArgs ++ [toFilePath file])
              CL.sinkNull
            exec menv (toExeName $ toFilePath file) (soArgs opts)
  where
    toPackageName = reverse . drop 1 . dropWhile (/= '-') . reverse

    -- Like words, but splits on both commas and spaces
    wordsComma = splitWhen (\c -> c == ' ' || c == ',')

    toExeName fp =
      if isWindows
        then replaceExtension fp "exe"
        else dropExtension fp

isWindows :: Bool
#ifdef WINDOWS
isWindows = True
#else
isWindows = False
#endif
