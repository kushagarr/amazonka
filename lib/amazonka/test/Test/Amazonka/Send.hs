-- |
-- Module      : Test.Amazonka.Send
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : provisional
-- Portability : non-portable (GHC extensions)
module Test.Amazonka.Send (tests) where

import Amazonka hiding (accept, error, runResourceT)
import qualified Amazonka.Auth as Auth
import qualified Amazonka.Data as Data
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import qualified Amazonka.STS as STS
import qualified Amazonka.Waiter as Waiter
import Control.Concurrent (ThreadId, forkFinally, forkIO, killThread)
import Control.DeepSeq (NFData (..))
import Control.Exception (ErrorCall (..), SomeException, bracket, displayException, try)
import Control.Monad (void)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString as ByteString
import qualified Network.HTTP.Client as Client
import Network.Socket
  ( Family (AF_INET),
    SockAddr (SockAddrInet),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    getSocketName,
    listen,
    setSocketOption,
    socket,
    tupleToHostAddress,
    withSocketsDo,
  )
import qualified Network.Socket.ByteString as Socket
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)
import Prelude

data DeepProbe = DeepProbe

instance Data.ToPath DeepProbe where
  toPath _ = "/"

instance Data.ToQuery DeepProbe where
  toQuery _ = mempty

instance Data.ToHeaders DeepProbe where
  toHeaders _ = mempty

data DeepResponse = DeepResponse () LazyPayload

instance NFData DeepResponse where
  rnf (DeepResponse metadata payload) = rnf metadata `seq` rnf payload

newtype LazyPayload = LazyPayload Int

instance NFData LazyPayload where
  rnf (LazyPayload value) = rnf value

instance AWSRequest DeepProbe where
  type AWSResponse DeepProbe = DeepResponse

  evaluateResponse _ = rnf

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBytes $ \_ _ _ ->
      Right (DeepResponse () (LazyPayload (error "deep response was forced")))

data ShallowProbe = ShallowProbe

newtype ShallowResponse = ShallowResponse (IO ())

instance Data.ToPath ShallowProbe where
  toPath _ = "/"

instance Data.ToQuery ShallowProbe where
  toQuery _ = mempty

instance Data.ToHeaders ShallowProbe where
  toHeaders _ = mempty

instance AWSRequest ShallowProbe where
  type AWSResponse ShallowProbe = ShallowResponse

  request overrides = Request.get (overrides STS.defaultService)

  response =
    Response.receiveBytes $ \_ _ _ ->
      Right (ShallowResponse (pure ()))

tests :: TestTree
tests =
  testGroup
    "Send response evaluation"
    [ testCase "sendUnsignedEither evaluates the selected response policy" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withEnv port $ \env ->
              runResourceT $ void (sendUnsignedEither env DeepProbe),
      testCase "awaitEither evaluates successful responses before acceptors" $
        withTestServer $ \port -> do
          assertDeepResponseForced $
            withSignedEnv port $ \env ->
              runResourceT $ void (awaitEither env successfulWait DeepProbe),
      testCase "the default response policy does not require NFData" $
        withTestServer $ \port -> do
          result <- try @SomeException $ withEnv port $ \env ->
            runResourceT $ void (sendUnsignedEither env ShallowProbe)
          case result of
            Left exception ->
              assertFailure $
                "expected the default response policy to succeed: "
                  <> displayException exception
            Right () -> pure ()
    ]

successfulWait :: Waiter.Wait DeepProbe
successfulWait =
  Waiter.Wait
    { Waiter.name = "deep-probe",
      Waiter.attempts = 1,
      Waiter.delay = 0,
      Waiter.acceptors = [\_ _ -> Just Waiter.AcceptSuccess]
    }

assertDeepResponseForced :: IO () -> IO ()
assertDeepResponseForced action = do
  result <- try @ErrorCall action
  case result of
    Left (ErrorCall message) ->
      assertEqual "unexpected evaluation exception" "deep response was forced" message
    Right () -> assertFailure "expected deep response evaluation to throw"

withEnv :: Int -> (EnvNoAuth -> IO a) -> IO a
withEnv port action = do
  manager <- Client.newManager Client.defaultManagerSettings
  env <- newEnvNoAuthFromManager manager
  let service = setEndpoint False "127.0.0.1" port STS.defaultService
  action (once (configureService service env))

withSignedEnv :: Int -> (Env -> IO a) -> IO a
withSignedEnv port action =
  withEnv port $
    action
      . Auth.fromKeys
        (AccessKey "test-access-key")
        (SecretKey "test-secret-key")

data TestServer = TestServer
  { serverSocket :: Socket,
    serverThread :: ThreadId,
    serverPort :: Int
  }

withTestServer :: (Int -> IO a) -> IO a
withTestServer action =
  withSocketsDo $
    bracket startServer stopServer (action . serverPort)

startServer :: IO TestServer
startServer = do
  sock <- socket AF_INET Stream defaultProtocol
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 0 (tupleToHostAddress (127, 0, 0, 1)))
  listen sock 1
  SockAddrInet port _ <- getSocketName sock
  thread <- forkIO $ do
    (connection, _) <- accept sock
    void . forkFinally (serve connection) $ const (close connection)
  pure
    TestServer
      { serverSocket = sock,
        serverThread = thread,
        serverPort = fromIntegral port
      }

stopServer :: TestServer -> IO ()
stopServer server = do
  killThread (serverThread server)
  close (serverSocket server)

serve :: Socket -> IO ()
serve connection = do
  receiveHeaders ByteString.empty
  Socket.sendAll
    connection
    "HTTP/1.1 200 OK\r\nContent-Length: 2\r\nConnection: close\r\n\r\nok"
  where
    receiveHeaders buffered
      | "\r\n\r\n" `ByteString.isInfixOf` buffered = pure ()
      | otherwise = do
          chunk <- Socket.recv connection 4096
          if ByteString.null chunk
            then pure ()
            else receiveHeaders (buffered <> chunk)
