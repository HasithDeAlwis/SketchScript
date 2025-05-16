import  { lazy, useRef, Suspense } from 'react';


export function MyEditor() {
  const editorRef = useRef(null);

  function handleEditorDidMount(editor, monaco) {
    editorRef.current = editor;
  }

  function showValue() {
    alert(editorRef.current.getValue());
  }

  return (
    <>
      <button onClick={showValue}>Show value</button>
    <Suspense fallback={<div>Loading...</div>}>

    </Suspense>
    </>
  );
}
