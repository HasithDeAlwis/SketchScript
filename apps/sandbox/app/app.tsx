import { downloadFile, fetchFilesByProjectId } from '@sandbox/api/files';
import { redirect } from 'react-router';
import { fetchProjectsByWorkspaceId } from '@sandbox/api/projects';
import { store } from '@sandbox/stores'; // import the Redux store directly
import { CodeEditor } from '@sandbox/features/code-editor';
import { EditorHeader } from '@sandbox/features/code-editor/ui/header';
import { FileMap } from '@sandbox/features/code-editor/types';

export async function clientLoader() {
  try {
    const state = store.getState();
    const currentWorkspace = state.workspaces.currentWorkspace;

    if (!currentWorkspace) {
      throw new Error('No workspace selected');
    }

    const projects = await fetchProjectsByWorkspaceId(
      currentWorkspace.workspace_id
    );
    const first = projects[0];
    if (!first) throw new Error('No projects found');

    const fileTree = await fetchFilesByProjectId(first.project_id);

    const fileMap: FileMap = {};
    for (const file of fileTree) {
      try {
        const fileData = await downloadFile(file.id)
        fileMap[file.id] = fileData;
      } catch (error) {
        console.error(`Failed to download file ${file.id}:`, error);
        fileMap[file.id] = ''; // Fallback to empty string if download fails}
      }
    }

    return {
      project: first,
      fileTree,
      fileMap
    };
  } catch (err) {
    console.error('Editor loader error:', err);
    return redirect('/login')
  }
}

export default function EditorPage() {
  return (
    <div className="flex flex-col h-screen">
      <EditorHeader />
      <CodeEditor />
    </div>
  );
}
