-- | 
-- Module      :  ImGui
-- Description :  A basic imgui binding - only necessarily works with the included cimgui
-- Copyright   :  (c) 2016 Caitlin Wilks
-- License     :  BSD3
-- Maintainer  :  Caitlin Wilks <mitasuki@gmail.com>
-- 
-- 

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module ImGui where

import Foreign.Ptr
import Foreign.C.String
import qualified Language.C.Inline as C

C.verbatim "#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS"
C.include "../../cimgui/cimgui/cimgui.h"
C.include "<stdio.h>"
C.include "<GL/gl.h>"
-- include c source directly or it causes a linker error with ghci..
C.include "cimgui_openglrender.h"


data InputData = InputData { mousePos :: (Float, Float)
                           , mouseDown :: (Bool, Bool, Bool, Bool, Bool)
                           , mouseWheel :: Float
                           , keyCtrl :: Bool
                           , keyShift :: Bool
                           , keyAlt :: Bool
                           , keySuper :: Bool
                           }


printThing :: IO ()
printThing = do
  x <- [C.exp| int{5*10*15*10*10} |]
  putStrLn . show $ x


initialiseGui :: IO ()
initialiseGui =
  [C.block|
    void {
      struct ImGuiIO* io = igGetIO();

      // Settings
      io->DisplaySize.x = 800;
      io->DisplaySize.y = 600;

      // Callbacks
      io->RenderDrawListsFn = renderGui;

      // Texture
      unsigned char* pix = 0;
      int w, h, bpp;
      ImFontAtlas_GetTexDataAsRGBA32(io->Fonts, &pix, &w, &h, &bpp);

      GLuint fontTex;
      glGenTextures(1, &fontTex);
      glBindTexture(GL_TEXTURE_2D, fontTex);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, pix);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      ImFontAtlas_SetTexID(io->Fonts, (void*)fontTex);
    }
  |]


newFrame :: InputData -> IO ()
newFrame inputData = do
  let InputData mousePos mouseDown mouseWheel keyCtrl keyShift keyAlt keySuper = inputData
  let (mouseX, mouseY) = (C.CFloat $ fst mousePos, C.CFloat $ snd mousePos)
  let (lmbDown, rmbDown, mmbDown, x1Down, x2Down) = mouseDown
  let cbool b = if b then C.CInt 1 else C.CInt 0
  let (lmbDownc, rmbDownc, mmbDownc, x1Downc, x2Downc) = (cbool lmbDown, cbool rmbDown, cbool mmbDown, cbool x1Down, cbool x2Down)
  [C.block|
    void {
      // Input
      struct ImGuiIO* io = igGetIO();
      io->MousePos.x = $(float mouseX);
      io->MousePos.y = $(float mouseY);
      io->MouseDown[0] = $(int lmbDownc);
      io->MouseDown[1] = $(int rmbDownc);
      io->MouseDown[2] = $(int mmbDownc);
      io->MouseDown[3] = $(int x1Downc);
      io->MouseDown[4] = $(int x2Downc);
    
      // Start new frame
      igNewFrame();
    }
  |]


renderGui :: IO ()
renderGui = do
  [C.block|
    void {
      igRender();
    }
  |]


igBegin :: String -> IO Bool
igBegin name = do
  b <- withCString name $ \n ->
         [C.block|
           int {
             bool open;
             igBegin($(const char* n), &open, 0);
             return open;
           }
         |]
  return (b /= 0)


igEnd :: IO ()
igEnd = do
  [C.block|
    void {
      igEnd();
    }
  |]


igText :: String -> IO ()
igText text = do
  withCString text $ \t ->
    [C.block|
      void {
        igText($(const char* t));
      }
    |]
