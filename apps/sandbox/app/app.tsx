import { fetchFilesByProjectId } from '@sandbox/api/files';
import { fetchProjectsByWorkspaceId } from '@sandbox/api/projects';
import { store } from '@sandbox/stores'; // import the Redux store directly
import { CodeEditor } from '@sandbox/features/code-editor';
import { EditorHeader } from '@sandbox/features/code-editor/ui/header';


export async function clientLoader() {
  try {
    const state = store.getState();
    const currentWorkspace = state.workspaces.currentWorkspace;

    if (!currentWorkspace) {
      throw new Error('No workspace selected');
    }

    const projects = await fetchProjectsByWorkspaceId(currentWorkspace.workspace_id);
    const first = projects[0];
    if (!first) throw new Error('No projects found');

    const fileTree = await fetchFilesByProjectId(first.project_id);

    return {
      project: first,
      fileTree,
    };
  } catch (err) {
    console.error('Editor loader error:', err);
    return null;
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
