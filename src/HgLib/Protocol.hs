{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}

module HgLib.Protocol
    ( -- * Protocol Operations
      openClient
    , closeClient
    , withClient
    , runCommand
    , rawCommand
    
    -- * Low-level Protocol
    , readHello
    , readChannel
    , writeCommand
    , Channel(..)
    , ChannelData(..)
    
    -- * Protocol Constants
    , inputFormatSize
    , outputFormatSize
    ) where

import Control.Exception (bracket, throwIO, finally)
import Control.Monad (when, void)
import Data.Binary.Get (Get, runGet, getWord32be, getWord8)
import Data.Binary.Put (Put, runPut, putWord32be, putByteString)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word32, Word8)
import System.Exit (ExitCode(..))
import System.IO (Handle, hClose, hFlush)
import System.Process (ProcessHandle, createProcess, proc, std_in, std_out, std_err, 
                      StdStream(..), waitForProcess, terminateProcess)
import System.Timeout (timeout)

import HgLib.Types
import HgLib.Error
import Logging

-- | Protocol constants
inputFormatSize :: Int
inputFormatSize = 4

outputFormatSize :: Int  
outputFormatSize = 5

-- | Channel types in the command server protocol
data Channel 
    = OutputChannel     -- ^ 'o' - command output
    | ErrorChannel      -- ^ 'e' - command errors  
    | ResultChannel     -- ^ 'r' - command result code
    | InputChannel      -- ^ 'I' - input request
    | LineInputChannel  -- ^ 'L' - line input request
    | DebugChannel      -- ^ 'd' - debug output
    | HelpChannel       -- ^ 'h' - help output
    deriving (Show, Eq)

-- | Channel data from server
data ChannelData = ChannelData
    { channelType :: !Channel
    , channelPayload :: !ByteString
    } deriving (Show, Eq)

-- | Parse channel from byte
parseChannel :: Word8 -> Either String Channel
parseChannel = \case
    111 -> Right OutputChannel     -- 'o'
    101 -> Right ErrorChannel      -- 'e' 
    114 -> Right ResultChannel     -- 'r'
    73  -> Right InputChannel      -- 'I'
    76  -> Right LineInputChannel  -- 'L'
    100 -> Right DebugChannel      -- 'd'
    104 -> Right HelpChannel       -- 'h'
    c   -> Left $ "Unknown channel: " ++ show (toEnum $ fromIntegral c :: Char)

-- | Convert channel to byte
channelToByte :: Channel -> Word8
channelToByte = \case
    OutputChannel    -> 111  -- 'o'
    ErrorChannel     -> 101  -- 'e'
    ResultChannel    -> 114  -- 'r'
    InputChannel     -> 73   -- 'I' 
    LineInputChannel -> 76   -- 'L'
    DebugChannel     -> 100  -- 'd'
    HelpChannel      -> 104  -- 'h'

-- | Open a new Mercurial client with the given configuration
openClient :: HgConfig -> IO HgClient
openClient config@HgConfig{..} = do
    let args = [hgHgPath, "serve", "--cmdserver", "pipe", "--config", "ui.interactive=True"]
            ++ maybe [] (\p -> ["-R", p]) hgPath
            ++ concatMap (\c -> ["--config", c]) hgConfigs
    
    let envs = [("HGPLAIN", "1")] ++ maybe [] (\e -> [("HGENCODING", e)]) hgEncoding
    
    (Just stdin_h, Just stdout_h, Just stderr_h, proc_h) <- 
        createProcess (proc hgHgPath (tail args)) 
            { std_in = CreatePipe
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
    logDebug $ "openClient: " ++ show hgHgPath ++ " " ++ show args ++ " env: " ++ show envs
    
    let client = HgClient proc_h stdin_h stdout_h stderr_h [] "" config Nothing hgDebug
    
    -- Read hello message and validate server
    readHello client

-- | Close the Mercurial client and return exit code
closeClient :: HgClient -> IO ExitCode
closeClient HgClient{..} = do
    logDebug "closeClient: starting cleanup"

    -- First, close stdin to signal the server to exit
    hClose clientStdin

    -- Give the process a moment to exit gracefully
    result <- timeout 5000000 (waitForProcess clientProcess)  -- 5 second timeout

    case result of
        Just exitCode -> do
            logDebug $ "closeClient: process exited with " ++ show exitCode
            hClose clientStdout
            hClose clientStderr
            return exitCode
        Nothing -> do
            logDebug "closeClient: timeout, terminating process"
            terminateProcess clientProcess
            hClose clientStdout
            hClose clientStderr
            return (ExitFailure 124)  -- time

-- closeClient HgClient{..} = do
--     hClose clientStdin `finally` do
--         hClose clientStdout `finally` do
--             hClose clientStderr `finally` do
--                 waitForProcess clientProcess

-- | Run an action with a Mercurial client, ensuring proper cleanup
withClient :: HgConfig -> (HgClient -> IO a) -> IO a
withClient config action = bracket (openClient config) closeClient action

-- | Read and parse the hello message from server
readHello :: HgClient -> IO HgClient
readHello client@HgClient{..} = do
    ChannelData channel msg <- readChannel client

    logDebug $ "readHello: channel: " ++ show channel ++ " msg: " ++ show msg

    when (channel == HelpChannel) $
        throwIO $ HgResponseError $ "Call to hg command shows help: Maybe wrong flag?" ++ show msg

    when (channel /= OutputChannel) $ 
        throwIO $ HgResponseError $ "Expected output channel in hello message:\n" ++ show msg
    
    let lines' = BS8.lines msg
    when (length lines' < 2) $ 
        throwIO $ HgResponseError "Incomplete hello message"
    
    let capLine = head lines'
        encLine = lines' !! 1
    
    -- Parse capabilities
    capabilities <- case BS8.stripPrefix "capabilities: " capLine of
        Just caps -> return $ BS8.words caps
        Nothing -> throwIO $ HgResponseError "Missing capabilities in hello message"
    
    -- Verify required capabilities
    when ("runcommand" `notElem` capabilities) $
        throwIO $ HgCapabilityError "Server does not support 'runcommand'"
    
    -- Parse encoding
    encoding <- case BS8.stripPrefix "encoding: " encLine of
        Just enc -> return enc
        Nothing -> throwIO $ HgResponseError "Missing encoding in hello message"
    
    return client 
        { clientCapabilities = capabilities
        , clientEncoding = encoding
        }

-- | Read a single channel message from the server
readChannel :: HgClient -> IO ChannelData
readChannel HgClient{..} = do
    -- Read 5-byte header: 1 byte channel + 4 bytes length
    header <- BS.hGet clientStdout outputFormatSize
    when (BS.length header < outputFormatSize) $
        throwIO $ HgResponseError "Incomplete channel header"
    
    let channelByte = BS.head header
        lengthBytes = BS.drop 1 header
        payloadLength = runGet getWord32be (LBS.fromStrict lengthBytes)
    
    channel <- case parseChannel channelByte of
        Right c -> return c
        Left err -> throwIO $ HgResponseError err
    
    -- For input channels, the "payload" is actually the length
    payload <- if channel `elem` [InputChannel, LineInputChannel]
        then return $ BS.pack [fromIntegral payloadLength]
        else BS.hGet clientStdout (fromIntegral payloadLength)
    
    logDebug $ "readChannel: channel: " ++ show channel ++ " -> " ++ show payload
    return $ ChannelData channel payload

-- | Write a command to the server
writeCommand :: HgClient -> [ByteString] -> IO ()
writeCommand HgClient{..} args = do
    -- Send "runcommand" header
    BS.hPut clientStdin "runcommand\n"
    
    -- Send command arguments
    let argData = BS.intercalate "\0" args
        lengthHeader = LBS.toStrict $ runPut $ putWord32be $ fromIntegral $ BS.length argData
    
    BS.hPut clientStdin lengthHeader
    BS.hPut clientStdin argData
    hFlush clientStdin
    logDebug $ "writeCommand: lengthHeader: " ++ show lengthHeader ++ " argData:" ++ show argData

-- | Run a command and collect all output
runCommand :: HgClient -> [ByteString] -> IO (ByteString, ByteString, Int)
runCommand client args = do
    writeCommand client args
    collectResponse client BS.empty BS.empty

-- | Collect response from command execution
collectResponse :: HgClient -> ByteString -> ByteString -> IO (ByteString, ByteString, Int)
collectResponse client stdout' stderr' = do
    ChannelData channel payload <- readChannel client
    
    case channel of
        OutputChannel -> 
            collectResponse client (stdout' <> payload) stderr'
        
        ErrorChannel -> 
            collectResponse client stdout' (stderr' <> payload)
        
        ResultChannel -> do
            let exitCode = fromIntegral $ runGet getWord32be (LBS.fromStrict payload)
            return (stdout', stderr', exitCode)
        
        InputChannel -> do
            -- Server requesting input - send empty response for now
            let responseLength = LBS.toStrict $ runPut $ putWord32be 0
            BS.hPut (clientStdin client) responseLength
            hFlush (clientStdin client)
            collectResponse client stdout' stderr'
        
        LineInputChannel -> do
            -- Server requesting line input - send empty line for now  
            let response = "\n"
                responseLength = LBS.toStrict $ runPut $ putWord32be $ fromIntegral $ BS.length response
            BS.hPut (clientStdin client) responseLength
            BS.hPut (clientStdin client) response
            hFlush (clientStdin client)
            collectResponse client stdout' stderr'
        
        DebugChannel -> do
            putStrLn $ "DebugChannel: " ++ show stdout' ++ ": " ++ show stderr'
            -- Ignore debug output and continue
            collectResponse client stdout' stderr'

        HelpChannel -> do
            -- TODO: formatting \n, \\n ...
            putStrLn $ "HelpChannel: " ++ show stdout' ++ ": " ++ show stderr'
            -- Ignore debug output and continue
            collectResponse client stdout' stderr'

-- | Run a raw command with error handling
rawCommand :: HgClient -> [ByteString] -> IO ByteString
rawCommand client args = do
    let debugArgs = args ++ (["--logfile=-" | clientDebug client && head args `elem` debugChannelCommands])
    (stdout', stderr', exitCode) <- runCommand client debugArgs
    logDebug $ "rawCommand: exitCode: " ++ show exitCode ++ " stdout: " ++ show stdout' ++ "stderr: " ++ show stderr'
    if exitCode == 0
        then return stdout'
        else throwIO $ HgCommandError 
            (map BS8.unpack args) exitCode stdout' stderr'

-- | Check if client has a specific capability
hasCapability :: HgClient -> ByteString -> Bool
hasCapability HgClient{..} capability = capability `elem` clientCapabilities

-- | Get server encoding
getEncoding :: HgClient -> ByteString  
getEncoding = clientEncoding

-- | Safely terminate client process
terminateClient :: HgClient -> IO ()
terminateClient HgClient{..} = do
    logDebug "terminateClient"
    terminateProcess clientProcess
