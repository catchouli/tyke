void renderGui(struct ImDrawData* d) {
  for (int i = 0; i < d->CmdListsCount; ++i) {
    struct ImDrawList* cmd_list = d->CmdLists[i];

    struct verts {
      float pos[2];
      float uv[2];
      int col;
    };

    int vertexCount = ImDrawList_GetVertexBufferSize(cmd_list);
    printf("vertexCount %d\n", vertexCount);
    struct verts* vertices = (struct verts*)ImDrawList_GetVertexPtr(cmd_list, 0);
    int indexCount = ImDrawList_GetIndexBufferSize(cmd_list);
    unsigned short* indices = (unsigned short*)ImDrawList_GetIndexPtr(cmd_list, 0);

    glClearColor(1.0f, 0.0f, 0.0f, 0.0f);
    glClear(GL_COLOR_BUFFER_BIT);

    glLoadIdentity();
    glOrtho(0.0f, 800.0f, 0.0f, 600.0f, -100.0f, 100.0f);
    glColor3f(0.0f, 1.0f, 0.0f);

    unsigned short* idx_buffer = indices;
    int cmdCount = ImDrawList_GetCmdSize(cmd_list);
    for (int j = 0; j < cmdCount; ++j) {
      struct ImDrawCmd* cmd = ImDrawList_GetCmdPtr(cmd_list, j);
      if (cmd->UserCallback) {
        cmd->UserCallback(cmd_list, cmd);
      }
      else {
        printf("render this command\n");

        glBindTexture(GL_TEXTURE_2D, (GLuint)cmd->TextureId);

        glBegin(GL_TRIANGLES);
        for (int i = 0; i < cmd->ElemCount; ++i) {
          unsigned short idx = idx_buffer[i];
          struct verts* vtx = &vertices[idx];
          glVertex2f(vtx->pos[0], vtx->pos[1]);
          glTexCoord2f(vtx->uv[0], vtx->uv[1]);
          unsigned char* col = (unsigned char*)&vtx->col;
          glColor3f(col[0] / 255.0f, col[1] / 255.0f, col[2] / 255.0f);
        }
        glEnd();
      }

      idx_buffer += cmd->ElemCount;
    }
  }
}
