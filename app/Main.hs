{-# LANGUAGE ForeignFunctionInterface #-}

import Foreign.Ptr (FunPtr, castPtrToFunPtr)
import Lib
import System.Win32.DLL (getProcAddress, loadLibraryEx)
import System.Win32.Types (Addr, nullHANDLE)

type TripleBool = Bool -> Bool -> Bool -> IO ()

foreign import ccall "dynamic" setSuspendState0 :: FunPtr TripleBool -> TripleBool

foreign import ccall "dynamic" lockWorkStation0 :: FunPtr (IO ()) -> IO ()

getFuncAddr :: String -> String -> IO Addr
getFuncAddr dllName methodName = do
  dll <- loadLibraryEx dllName nullHANDLE 0x00000800
  getProcAddress dll methodName

-- true: hibernate
-- false: sleep
setSuspendState :: Bool -> IO ()
setSuspendState b1 = do
  addr <- getFuncAddr "PowrProf.dll" "SetSuspendState"
  setSuspendState0 (castPtrToFunPtr addr) b1 False False

lockWorkStation :: IO ()
lockWorkStation = do
  addr <- getFuncAddr "User32.dll" "LockWorkStation"
  lockWorkStation0 $ castPtrToFunPtr addr

main :: IO ()
-- main = setSuspendState True
main = lockWorkStation

-- main = mainLoop
