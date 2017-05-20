void renderGui(struct ImDrawData* d) {
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

    glLoadIdentity();
    glOrtho(0.0f, 800.0f, 600.0f, 0.0f, -100.0f, 100.0f);
    glColor3f(0.0f, 1.0f, 0.0f);
    glDisable(GL_CULL_FACE);
    glDisable(GL_DEPTH_TEST);
    glEnable(GL_BLEND);
    glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

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
      if (cmd->UserCallback) {
        cmd->UserCallback(cmd_list, cmd);
      }
      else {
        glBindTexture(GL_TEXTURE_2D, (GLuint)cmd->TextureId);
        glDrawElements(GL_TRIANGLES, cmd->ElemCount, GL_UNSIGNED_SHORT, idx_buffer);
      }
      idx_buffer += cmd->ElemCount;
    }

    glDisable(GL_TEXTURE_2D);
    glDisableClientState(GL_VERTEX_ARRAY);
  }
}
