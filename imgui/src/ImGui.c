
#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS

#define STB_IMAGE_WRITE_IMPLEMENTATION

#include "../../cimgui/cimgui/cimgui.h"

#include <stdio.h>

#include <GL/gl.h>

#include "cimgui_openglrender.inl"

#include "stb_image_write.h"

int inline_c_ImGui_0_52f02ec7bdae81697a43b4b9d11802bc16151a55() {
return (5*10*15*10*10);
}


void inline_c_ImGui_1_cdb88f3c4f04ef2db62740867cbc12262478c5c8() {

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
      //for (int i = 0; i < w * h; ++i) {
      //  int y = i / w;
      //  printf("%d\n", y);
      //  ((unsigned int*)pix)[i] = ((y > 30 && y < 40) ? 0xFFFF00FF : 0xFF00FF00);
      //}
      printf("ptr: %d %d %d %d\n", pix, bpp, w, h);
      stbi_write_bmp("out.png", w, h, 4, pix);

      GLuint fontTex;
      glGenTextures(1, &fontTex);
      glBindTexture(GL_TEXTURE_2D, fontTex);
      glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, w, h, 0, GL_RGBA, GL_UNSIGNED_BYTE, pix);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
      glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);

      //printf("creating texture: %d\n", fontTex);
      //
      //

      ImFontAtlas_SetTexID(io->Fonts, (void*)fontTex);
    
}


void inline_c_ImGui_2_e6c39c8c2ceed5c9e972ea8ed96220403aada517(float mouseX_inline_c_0, float mouseY_inline_c_1, int lmbDownc_inline_c_2, int rmbDownc_inline_c_3, int mmbDownc_inline_c_4, int x1Downc_inline_c_5, int x2Downc_inline_c_6) {

      // Input
      struct ImGuiIO* io = igGetIO();
      io->MousePos.x = mouseX_inline_c_0;
      io->MousePos.y = mouseY_inline_c_1;
      io->MouseDown[0] = lmbDownc_inline_c_2;
      io->MouseDown[1] = rmbDownc_inline_c_3;
      io->MouseDown[2] = mmbDownc_inline_c_4;
      io->MouseDown[3] = x1Downc_inline_c_5;
      io->MouseDown[4] = x2Downc_inline_c_6;
      //
    
      // Start new frame
      igNewFrame();
      
      // test gui
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


void inline_c_ImGui_3_09ebaf299149b8ab03cc3f0bc1fbeb95d4a455a1() {

      igRender();
      //
        //
    
}

