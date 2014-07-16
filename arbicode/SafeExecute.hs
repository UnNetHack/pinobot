{-# LANGUAGE OverloadedStrings, ViewPatterns, DeriveDataTypeable #-}
{-# LANGUAGE LambdaCase, TemplateHaskell, ForeignFunctionInterface #-}

module SafeExecute
    ( safeExecute )
    where

import Data.Typeable
import Control.Applicative
import Control.Lens
import Control.Exception
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as TL
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as M
import qualified Data.ByteString as B
import Control.Monad.Trans.Writer.Strict
import System.IO.Error
import Data.Attoparsec.Text
import Data.Monoid
import Data.Char
import System.Process
import System.Process.Internals hiding ( fdToHandle )
import System.Timeout
import System.Exit
import Data.Maybe
import System.Posix.Signals
import System.Posix.Process.ByteString
import System.Posix.IO
import System.Posix.Directory.ByteString
import System.IO
import System.Posix.Types
import Safe ( readMay )

foreign import ccall unsafe "masochist" c_masochist :: IO ()

data ExecutionEnv = MkExecutionEnv
    { _definitions :: !(M.Map T.Text ([T.Text], T.Text))
    , _definitionTypes :: !(M.Map T.Text T.Text) }
    deriving ( Eq, Ord, Show, Read, Typeable )
makeLenses ''ExecutionEnv

emptyExecutionEnv :: ExecutionEnv
emptyExecutionEnv = MkExecutionEnv
    { _definitions = M.empty
    , _definitionTypes = M.empty }

data BailOut = BailOut T.Text
               deriving ( Eq, Ord, Show, Read, Typeable )

instance Exception BailOut

safeExecute :: T.Text -> IO T.Text
safeExecute txt =
    flip catchIOError (\ierr -> return $ T.pack $ show ierr) $
    handle (\(BailOut bo) -> return bo) (safeExecute' txt)

safeExecute' :: T.Text -> IO T.Text
safeExecute' (T.strip -> txt)
    | T.null txt = return "Nothing to do."
    | T.head txt == '%' = makeDefinition (T.tail txt)
    | T.head txt == '!' = removeDefinitions (T.tail txt)
    | T.head txt == '?' = checkDefinition (T.tail txt)
    | T.take 2 txt == "::" = makeTypeDefinition (T.drop 2 txt)
    | otherwise = makeExecution txt

checkDefinition :: T.Text -> IO T.Text
checkDefinition (T.strip -> name) = do
    env <- readExecutionEnv
    pure $ case M.lookup name (_definitions env) of
        Nothing -> "Not defined."
        Just (args, x)  ->
            case M.lookup name (_definitionTypes env) of
                Just t -> "(" <> name <> " :: " <> t <> ") " <> layoutNames args <> " = " <> x
                Nothing -> name <> " " <> layoutNames args <> " = " <> x

layoutNames :: [T.Text] -> T.Text
layoutNames [] = ""
layoutNames (x:rest) =
    x <> mconcat (fmap (\y -> " " <> y) rest)

readExecutionEnv :: IO ExecutionEnv
readExecutionEnv =
    catchIOError rest (\_ -> return emptyExecutionEnv)
  where
    rest = readMay . T.unpack <$> T.readFile "arbicode.var" >>= \case
        Nothing -> return emptyExecutionEnv
        Just env -> return env

writeExecutionEnv :: ExecutionEnv -> IO ()
writeExecutionEnv env = do
    when (M.size (_definitions env) > 30 ||
          M.size (_definitionTypes env) > 30) $
        throwIO $ BailOut "Cannot add more definitions."
    when (isJust $ M.lookup "main" (_definitions env)) $
        throwIO $ BailOut "Cannot define 'main'."
    when (isJust $ M.lookup "code__" (_definitions env)) $
        throwIO $ BailOut "Cannot define 'code__'."
    T.writeFile "arbicode.var" (T.pack $ show env)

removeDefinitions :: T.Text -> IO T.Text
removeDefinitions txt = do
    env <- readExecutionEnv
    let new_env = env & (definitions.at txt .~ Nothing) .
                        (definitionTypes.at txt .~ Nothing)
    attemptCompilation new_env Nothing
    writeExecutionEnv new_env
    return $ if M.member txt (_definitions env) ||
                M.member txt (_definitionTypes env)
               then "Removed definition."
               else "Nothing to remove."

parseId :: Parser T.Text
parseId = do
    ch <- letter
    rest <- many (letter <|> digit <|> satisfy (inClass "_'"))
    return $ T.pack (ch:rest)

parseArgs :: Parser [T.Text]
parseArgs = rec []
  where
    rec accum = do
        void $ many space
        ch <- peekChar'
        if ch == '=' then return (reverse accum)
                     else do x <- (T.pack <$> (many1 "_" *> many (letter <|> digit <|> satisfy (inClass "_'")))
                                      <|> parseId)
                             rec (x:accum)

defParse :: Parser (T.Text, [T.Text], T.Text)
defParse = do
    name <- parseId
    args <- parseArgs
    void $ many space *> char '=' *> many space
    definition <- takeText
    return (T.strip name, fmap T.strip args, T.strip definition)

parseDefinition :: T.Text -> IO (T.Text, [T.Text], T.Text)
parseDefinition txt =
    case parseOnly defParse txt of
        Left err -> throwIO $ BailOut $ T.pack err
        Right result -> pure result

defTypeParse :: Parser (T.Text, T.Text)
defTypeParse = do
    name <- parseId
    void $ many space *> string "::" *> many space
    definition <- takeText
    return (T.strip name, T.strip definition)

parseTypeDefinition :: T.Text -> IO (T.Text, T.Text)
parseTypeDefinition txt =
    case parseOnly defTypeParse txt of
        Left err -> throwIO $ BailOut $ T.pack err
        Right result -> pure result

makeDefinition :: T.Text -> IO T.Text
makeDefinition txt = do
    env <- readExecutionEnv
    (name, args, def) <- parseDefinition txt
    let new_env = env & definitions.at name .~ Just (args, def)
    attemptCompilation new_env Nothing
    writeExecutionEnv new_env
    case M.lookup name (_definitions env) of
        Nothing -> return $ "Defined: " <> name
        Just _ -> return $ "Re-defined: " <> name

attemptRun :: ExecutionEnv -> T.Text -> IO T.Text
attemptRun env txt = do
    attemptCompilation env (Just txt)
    T.writeFile "arbicode-sandbox/Main.hs" $ runCodeGeneration
    runGHC ["Env.hs", "Main.hs"]
    (read_pipe, write_pipe) <- createPipe
    str <- flip finally (closeFd write_pipe) $ do
        pid <- forkProcess $ do
            closeFd (Fd 1)
            closeFd (Fd 2)
            dupTo write_pipe (Fd 1)
            dupTo write_pipe (Fd 2)
            changeWorkingDirectory "./arbicode-sandbox"
            c_masochist
            executeFile (T.encodeUtf8 "./Main") False [] Nothing
            exitFailure
        result <- timeout 5000000 $ do
            Just status <- getProcessStatus True False pid
            return status
        case result of
            Nothing -> do
                closeFd read_pipe
                closeFd write_pipe
                signalProcess sigKILL pid
                void $ getProcessStatus True False pid
                throwIO $ BailOut "Timeout."
            Just _ -> do
                hndl <- fdToHandle read_pipe
                str <- B.hGetNonBlocking hndl 1024
                if str == ""
                  then throwIO $ BailOut "Expired."
                  else case fmap cmap (T.decodeUtf8' str) of
                           Left _ -> throwIO $
                               BailOut "Invalid UTF-8 in result."
                           Right x -> return x

    return str
  where
    cmap = T.map (\ch -> if isSpace ch || ord ch < 32 then ' ' else ch)

runCodeGeneration :: T.Text
runCodeGeneration = TL.toStrict . TL.toLazyText $ execWriter $ do
    tell "module Main ( main ) where\n"
    tell "import Env (__code)\n"
    tell "main :: IO ()\n"
    tell $ "main = putStrLn $ if length (take 201 __code) /= length (take 200 __code) then take 200 __code ++ \"...\" else __code\n"

withProcessTimeout :: Int -> ProcessHandle -> Handle -> T.Text -> IO ()
withProcessTimeout useconds phandle stderr err_msg = do
    result <- timeout useconds $ do
        code <- waitForProcess phandle
        when (code /= ExitSuccess) $ do
            errs <- T.hGetContents stderr
            throwIO $ BailOut $ err_msg <> cmap errs

    when (isNothing result) $ do
        withProcessHandle phandle $ \case
            OpenHandle cpid -> signalProcess sigKILL cpid
            _ -> pure ()
        _ <- waitForProcess phandle
        throwIO $ BailOut "Timeout."
  where
    cmap = T.map (\ch -> if isSpace ch then ' ' else ch)

runGHC :: [T.Text] -> IO ()
runGHC file_name = do
    (_, _, Just stderr, phandle) <- createProcess cproc
    withProcessTimeout 5000000 phandle stderr "Compilation failed:"
  where
    cproc = CreateProcess
        { cmdspec = RawCommand "../arbicode-sandbox-bin/ghc"
                               $ ["--make"] <> fmap T.unpack file_name
        , cwd = Just "arbicode-sandbox"
        , env = Nothing
        , std_in = Inherit
        , std_out = CreatePipe
        , std_err = CreatePipe
        , close_fds = True
        , create_group = False
        , delegate_ctlc = False }

attemptCompilation :: ExecutionEnv -> Maybe T.Text -> IO ()
attemptCompilation env opt_code = do
    T.writeFile "arbicode-sandbox/Env.hs" code
    runGHC ["Env.hs"]
  where
    code = codeGeneration env <>
        case opt_code of
            Nothing -> ""
            Just c -> "__code = showP (" <> c <> ")\n"

codeGeneration :: ExecutionEnv -> T.Text
codeGeneration env = TL.toStrict . TL.toLazyText $ execWriter $ do
    tell "{-# LANGUAGE Safe #-}\n"
    tell "{-# LANGUAGE UnicodeSyntax #-}\n"
    tell "{-# LANGUAGE NoImplicitPrelude #-}\n"
    tell "{-# LANGUAGE OverloadedStrings #-}\n"
    tell "{-# LANGUAGE ViewPatterns #-}\n"
    tell "{-# LANGUAGE LambdaCase #-}\n"
    tell "{-# LANGUAGE MultiWayIf #-}\n"
    tell "{-# LANGUAGE RankNTypes #-}\n"
    tell "{-# LANGUAGE ScopedTypeVariables #-}\n"
    tell "{-# LANGUAGE AutoDeriveTypeable #-}\n"
    tell "module Env where\n"
    imports
    iforOf_ ifolded (_definitions env) $ \name (args, def) -> do
        case M.lookup name (_definitionTypes env) of
            Nothing -> pure ()
            Just x -> tell $ cmap name <> " :: " <> cmap x <> "\n"
        tell $ cmap name <> " " <> (TL.fromText $ layoutNames args) <> " = " <> cmap def <> "\n"
  where
    cmap = TL.fromText . T.map (\ch -> if isSpace ch then ' ' else ch)

imports :: Writer TL.Builder ()
imports = do
    tell "import PinobotSafe\n"
    tell "import qualified NetHack.Imported.Vanilla as Vanilla\n"
    tell "import qualified NetHack.Imported.Brass as Brass\n"
    tell "import qualified NetHack.Imported.GruntHack as GruntHack\n"
    tell "import qualified NetHack.Imported.SlashemExtended as SlashemExtended\n"
    tell "import qualified NetHack.Imported.SporkHack as SporkHack\n"
    tell "import qualified NetHack.Imported.UnNetHackPlus as UnNetHackPlus\n"
    tell "import qualified NetHack.Imported.Dnethack as Dnethack\n"
    tell "import qualified NetHack.Imported.Slashem as Slashem\n"
    tell "import qualified NetHack.Imported.UnNetHack as UnNetHack\n"

makeTypeDefinition :: T.Text -> IO T.Text
makeTypeDefinition txt = do
    env <- readExecutionEnv
    (name, tdef) <- parseTypeDefinition txt
    let new_env = env & (definitionTypes.at name .~ Just tdef) .
                        (if M.member name (_definitions env)
                           then id
                           else definitions.at name .~ Just ([], "undefined"))
    attemptCompilation new_env Nothing
    writeExecutionEnv new_env
    case M.lookup name (_definitions env) of
        Nothing -> return ("Type defined (default body given): " <> name)
        Just _ -> return $ "Type defined: " <> name

makeExecution :: T.Text -> IO T.Text
makeExecution txt = do
    env <- readExecutionEnv
    attemptRun env txt

