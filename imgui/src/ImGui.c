
#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS

#include "../../cimgui/cimgui/cimgui.h"

#include <stdio.h>

#include <GL/gl.h>

#include "cimgui_openglrender.inl"

int inline_c_ImGui_0_52f02ec7bdae81697a43b4b9d11802bc16151a55() {
return (5*10*15*10*10);
}


void inline_c_ImGui_1_3cfd709122091a297ac9cbbe0f079fabea3370c5() {

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


void inline_c_ImGui_2_159a746c85e20c2a1824e238a5d01a749c9aef67() {

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


void inline_c_ImGui_3_587f643316c85f0452f9dd430ce8e09be30abcac() {

      //glClearColor(1.0f, 0.0f, 1.0f, 1.0f);
      //glClear(GL_COLOR_BUFFER_BIT);
      printf("creating label text\n");
      printf("rendering\n");
      igRender();
    
}

