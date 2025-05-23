import { useState, useRef } from 'react'
import { FileTree } from './file-tree'
import { Canvas } from '../canvas/ui'
import { MyEditor } from './editor'

export function CodeEditor() {
  const treeRef = useRef(null)
  const [treeWidth, setTreeWidth] = useState(210)



  return (
    <div className="flex h-screen p-2 overflow-hidden max-w-screen">
      <FileTree treeRef={treeRef} treeWidth={treeWidth} setTreeWidth={setTreeWidth} />
      <MyEditor />
      <Canvas htmlOutput={`<div className='text-lg'><h1>Hello world!</h1><p>This is your rendered DSL.</p></div>`} />
    </div>
  )
}
