import { Editor } from '@monaco-editor/react';
import { RootState } from '../../../stores/index';
import { options, handleEditorDidMount } from '../hook/config';
import { useSelector } from 'react-redux';
import { Skeleton } from '@shared/ui/skeleton';
import { uploadFile } from '../../../api/files'
import { useDebouncedMutation } from '../../../shared/useDebounceHook';
import { FileMap } from '../types';

function EditorLoadingFallback() {
  return (
    <div className="flex items-center justify-center w-full h-screen bg-primary/50">
      <Skeleton className="w-11/12 rounded-lg h-11/12" />
    </div>
  );
}

export function MyEditor({ allFiles, currentNode, setAllFiles }: { allFiles: FileMap, currentNode: string, setAllFiles: React.Dispatch<React.SetStateAction<FileMap>> }) {
  const theme = useSelector((state: RootState) => state.theme);
  const monacoTheme =
    theme === 'light' ? 'sketch-script-light' : 'sketch-script-dark';

  const debouncedUpload = useDebouncedMutation(uploadFile, 500);

  return (
    <div className="w-full h-screen">
      <Editor
        loading={<EditorLoadingFallback />}
        options={options}
        height="100%"
        width="100%"
        defaultLanguage="sketch-script"
        theme={monacoTheme}
        beforeMount={(params) => handleEditorDidMount(params)}
        value={allFiles[currentNode] || ''}
        onChange={(value) => {
          debouncedUpload(value || '', currentNode, setAllFiles)
        }}
      />
    </div>
  );
}
