import Control.Concurrent
import Text.Printf
import Control.Monad

main =
  forever $ do
  	s <- getLine           -- 1
	if s == "exit"
		then return (forkIO $ return ())
		else return (forkIO $ setReminder s)

setReminder :: String -> IO ()
setReminder s  = do
	let t = read s :: Int
      	printf "Ok, I'll remind you in %d seconds\n" t
       	threadDelay (10^6 * t)                   -- 3
	printf "%d seconds is up! BING!\BEL\n" t -- 4
