import Data.Char (ord)

--encrypts a given string
--returns the encrypted string and a function to decrypt the string
encrypt :: Int -> String -> (String, String -> String)
encrypt key str = (enc, \str -> [negate key `applyKey` x | x <- str])
    where
    	--applies a key to a character
    	applyKey :: Int -> Char -> Char
        applyKey k c | k + ord c > 122 = toEnum $ k + ord c - 26
                     | k + ord c < 97 = toEnum $ k + ord c + 26
                     | otherwise = toEnum $ k + ord c

    	--encrypts a string with an int
        enc :: String
        enc = [applyKey key x | x <-str]