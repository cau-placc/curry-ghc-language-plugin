{-# OPTIONS_GHC -fplugin Plugin.ParsePlugin #-}
module Example where

parseAnBn :: Maybe Integer
parseAnBn = goAn 0
  where 
    goAn :: Integer -> Maybe Integer
    goAn n | isEOF     = goBn n 0
           | otherwise = case anyChar of
                          'a' -> goAn (n+1)
                          'b' -> goBn n 1
                          _   -> Nothing
    goBn :: Integer -> Integer -> Maybe Integer
    goBn n m | n == m    = Just n
             | isEOF     = Nothing 
             | otherwise = case anyChar of 
                            'b' -> goBn n (m+1)
                            _   -> Nothing
