-- import Control.Exception

-- main = catch (print $ 5 `div` 0) handler
--   where
--     handler :: SomeException -> IO ()
--     handler ex = putStrLn $ "Caught exception: " ++ show ex


import qualified Control.Exception as Exc

{-# NOINLINE unsafeCleanup #-}
unsafeCleanup :: a -> Maybe a
unsafeCleanup x = unsafePerformIO $ Exc.catch (x `seq` return (Just x)) handler
    where
    handler exc = return Nothing  `const`  (exc :: Exc.ErrorCall)