import { Editor } from '@monaco-editor/react';
import { options, handleEditorDidMount } from '../hook/config';
import { Skeleton } from '@shared/ui/skeleton';

function EditorLoadingFallback() {
  return (
    <div className="flex items-center justify-center w-full h-screen bg-primary/50">
      <Skeleton className="w-11/12 rounded-lg h-11/12" />
    </div>
  );
}

export function MyEditor() {
  return (
    <div className="w-full h-screen">
      <Editor
        loading={<EditorLoadingFallback />}
        options={options}
        height="100%"
        width="100%"
        defaultLanguage="ui-script"
        defaultValue=""
        theme="sketch-script-dark"
        beforeMount={handleEditorDidMount}
      />
    </div>
  );
}
