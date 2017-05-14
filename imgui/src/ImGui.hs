{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui where

import qualified Language.C.Inline as C

C.include "<stdio.h>"

printThing :: IO ()
printThing = do
  x <- [C.exp| int{5*10*15*10*10} |]
  putStrLn . show $ x
