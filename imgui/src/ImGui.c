
#define CIMGUI_DEFINE_ENUMS_AND_STRUCTS

#include "../../cimgui/cimgui/cimgui.h"

#include <stdio.h>

#include <GL/gl.h>

#include "cimgui_openglrender.h"

int inline_c_ImGui_0_52f02ec7bdae81697a43b4b9d11802bc16151a55() {
return (5*10*15*10*10);
}


void inline_c_ImGui_1_bb603511e96a8b085ef6d2dda227638c89fd3ffb() {

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


void inline_c_ImGui_2_7b444b45ed81f292e1a86beeb65cc9d6b7ebf271(float mouseX_inline_c_0, float mouseY_inline_c_1, int lmbDownc_inline_c_2, int rmbDownc_inline_c_3, int mmbDownc_inline_c_4, int x1Downc_inline_c_5, int x2Downc_inline_c_6) {

      // Input
      struct ImGuiIO* io = igGetIO();
      io->MousePos.x = mouseX_inline_c_0;
      io->MousePos.y = mouseY_inline_c_1;
      io->MouseDown[0] = lmbDownc_inline_c_2;
      io->MouseDown[1] = rmbDownc_inline_c_3;
      io->MouseDown[2] = mmbDownc_inline_c_4;
      io->MouseDown[3] = x1Downc_inline_c_5;
      io->MouseDown[4] = x2Downc_inline_c_6;
    
      // Start new frame
      igNewFrame();
    
}


void inline_c_ImGui_3_ccb45e38de6c3492fffc1a1b28df0121c098e349() {

      igRender();
    
}


int inline_c_ImGui_4_44de12c5a3b5c6130ba49e8f1c2d76724066337e(const char * n_inline_c_0) {

             bool open;
             igBegin(n_inline_c_0, &open, 0);
             return open;
           
}


void inline_c_ImGui_5_a6af349ca90cd012f388eb8324797acafa0d6ad5() {

      igEnd();
    
}


void inline_c_ImGui_6_a183aee5b69591ddc44bda9a6c21f4c873fa7e86(const char * t_inline_c_0) {

        igText(t_inline_c_0);
      
}

