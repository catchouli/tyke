#include <stdio.h>
#include <GL/gl.h>

void renderGui(struct ImDrawData* d) {
  struct ImGuiIO* io = igGetIO();
  int fb_width = (int)(io->DisplaySize.x * io->DisplayFramebufferScale.x);
  int fb_height = (int)(io->DisplaySize.y * io->DisplayFramebufferScale.y);

  glMatrixMode(GL_MODELVIEW);
  glLoadIdentity();
  glMatrixMode(GL_PROJECTION);
  glLoadIdentity();

  glPushAttrib(GL_ALL_ATTRIB_BITS);
  glPushClientAttrib(GL_CLIENT_ALL_ATTRIB_BITS);

  for (int i = 0; i < d->CmdListsCount; ++i) {
    struct ImDrawList* cmd_list = d->CmdLists[i];

    struct verts {
      float pos[2];
      float uv[2];
      int col;
    };

    int vertexCount = ImDrawList_GetVertexBufferSize(cmd_list);
    struct verts* vertices = (struct verts*)ImDrawList_GetVertexPtr(cmd_list, 0);
    int indexCount = ImDrawList_GetIndexBufferSize(cmd_list);
    unsigned short* indices = (unsigned short*)ImDrawList_GetIndexPtr(cmd_list, 0);

    glUseProgram(0);
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    glOrtho(0.0f, (float)fb_width, (float)fb_height, 0.0f, -100.0f, 100.0f);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
    glEnable(GL_SCISSOR_TEST);

    glEnableClientState(GL_VERTEX_ARRAY);
    glEnableClientState(GL_TEXTURE_COORD_ARRAY);
    glEnableClientState(GL_COLOR_ARRAY);
    glVertexPointer(2, GL_FLOAT, sizeof(struct verts), (void*)vertices);
    glTexCoordPointer(2, GL_FLOAT, sizeof(struct verts), (void*)vertices + 2 * sizeof(float));
    glColorPointer(4, GL_UNSIGNED_BYTE, sizeof(struct verts), (void*)vertices + 4 * sizeof(float));
    glEnable(GL_TEXTURE_2D);

    unsigned short* idx_buffer = indices;
    int cmdCount = ImDrawList_GetCmdSize(cmd_list);
    for (int j = 0; j < cmdCount; ++j) {
      struct ImDrawCmd* cmd = ImDrawList_GetCmdPtr(cmd_list, j);

      glScissor((int)cmd->ClipRect.x, (int)(fb_height - cmd->ClipRect.w), (int)(cmd->ClipRect.z - cmd->ClipRect.x), (int)(cmd->ClipRect.w - cmd->ClipRect.y));

      if (cmd->UserCallback) {
        cmd->UserCallback(cmd_list, cmd);
      }
      else {
        glBindTexture(GL_TEXTURE_2D, (GLuint)cmd->TextureId);
        //glDrawElements(GL_TRIANGLES, cmd->ElemCount, GL_UNSIGNED_SHORT, idx_buffer);
        
        // couldn't get this glDrawElements working properly
        // it would break whenever the lambdacube renderer was enabled
        // and i don't know why... immediate mode works though
        glBegin(GL_TRIANGLES);
        for (int i = 0; i < cmd->ElemCount; ++i) {
          unsigned short idx = idx_buffer[i];
          struct verts* vert = &vertices[idx];
          unsigned char* col = (unsigned char*)&vert->col;
          glColor4f(col[0]/255.0f, col[1]/255.0f, col[2]/255.0f, col[3]/255.0f);
          glTexCoord2f(vert->uv[0], vert->uv[1]);
          glVertex2f(vert->pos[0], vert->pos[1]);
        }
        glEnd();
      }
      idx_buffer += cmd->ElemCount;
    }

    glDisable(GL_TEXTURE_2D);
    glDisableClientState(GL_VERTEX_ARRAY);
    glDisableClientState(GL_TEXTURE_COORD_ARRAY);
    glDisableClientState(GL_COLOR_ARRAY);
    glDisable(GL_BLEND);
    glDisable(GL_SCISSOR_TEST);
  }

  glPopAttrib();
  glPopClientAttrib();
}

