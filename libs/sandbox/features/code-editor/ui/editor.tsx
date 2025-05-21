import { Editor } from '@monaco-editor/react';
import { RootState } from '../../../stores/index';
import { options, handleEditorDidMount } from '../hook/config';
import { useSelector } from 'react-redux';
import { Skeleton } from '@shared/ui/skeleton';

function EditorLoadingFallback() {
  return (
    <div className="flex items-center justify-center w-full h-screen bg-primary/50">
      <Skeleton className="w-11/12 rounded-lg h-11/12" />
    </div>
  );
}

export function MyEditor() {
  const theme = useSelector((state: RootState) => state.theme);

  const monacoTheme =
    theme === 'light' ? 'sketch-script-light' : 'sketch-script-dark';

  return (
    <div className="w-full h-screen">
      <Editor
        loading={<EditorLoadingFallback />}
        options={options}
        height="100%"
        width="100%"
        defaultLanguage="ui-script"
        defaultValue=""
        theme={monacoTheme}
        beforeMount={handleEditorDidMount}
      />
    </div>
  );
}
