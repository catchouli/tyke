{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui where

import qualified Language.C.Inline as C

C.include "<stdio.h>"
C.include "<GL/gl.h>"

printThing :: IO ()
printThing = do
  x <- [C.exp| int{5*10*15*10*10} |]
  putStrLn . show $ x


renderGui :: IO ()
renderGui = do
  [C.block|
    void {
      glClearColor(1.0f, 0.0f, 1.0f, 1.0f);
      glClear(GL_COLOR_BUFFER_BIT);
    }
  |]
