import { Suspense } from 'react';
import { Editor } from '@monaco-editor/react'
import { options, handleEditorDidMount } from '../hook/config'

export default function MyEditor() {


  return (
    <>
      <Suspense fallback={<div>Loading editor...</div>}>
        <div className='h-screen w-screen'>

        <Editor
          options={options}
          height="100%"
          width="100%"
          defaultLanguage="ui-script"
          defaultValue=""
          theme="sketch-script-dark"
          beforeMount={handleEditorDidMount}
        />
        </div>
      </Suspense>
    </>
  );
}
