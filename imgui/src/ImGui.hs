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
import qualified Language.C.Inline as C

C.verbatim "#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS"
C.include "../../cimgui/cimgui/cimgui.h"
C.include "<stdio.h>"
C.include "<GL/gl.h>"
C.include "cimgui_openglrender.inl"

printThing :: IO ()
printThing = do
  x <- [C.exp| int{5*10*15*10*10} |]
  putStrLn . show $ x


initialiseGui :: IO ()
initialiseGui =
  [C.block|
    void {
      printf("initialising gui\n");
      struct ImGuiIO* io = igGetIO();

      // Settings
      io->DisplaySize.x = 800;
      io->DisplaySize.y = 600;

      // Callbacks
      io->RenderDrawListsFn = renderGui;

      // Texture
      unsigned char* pix;
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


newFrame :: IO ()
newFrame = do
  putStrLn "newFrame"
  [C.block|
    void {
      printf("new frame\n");
      igNewFrame();
//
  bool i;
      igBegin("a", &i, 0);
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igLabelText("test", "AAA");
      igEnd();
    }
  |]


renderGui :: IO ()
renderGui = do
  putStrLn "renderGui"
  [C.block|
    void {
      //glClearColor(1.0f, 0.0f, 1.0f, 1.0f);
      //glClear(GL_COLOR_BUFFER_BIT);
      printf("creating label text\n");
      printf("rendering\n");
      igRender();
    }
  |]
