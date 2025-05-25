import { useState, useRef } from 'react'
import { FileTree } from './file-tree'
import { Canvas } from '../canvas/ui'
import { MyEditor } from './editor'
import { FileMap } from '../types';
import { useLoaderData } from 'react-router';
import { FileTreeNode } from '../../../api/files';


export function CodeEditor() {
  const { fileTree } = useLoaderData() as { fileTree: FileTreeNode[] }

  const treeRef = useRef(null)
  const [treeWidth, setTreeWidth] = useState(210)
  const [allFiles, setAllFiles] = useState<FileMap>({})
  const [currentFile, setCurrentFile] = useState<string>(fileTree[0].id)

  return (
    <div className="flex h-screen p-2 overflow-hidden max-w-screen">
      <FileTree treeRef={treeRef} treeWidth={treeWidth} setTreeWidth={setTreeWidth} setCurrentFile={setCurrentFile} />
      <MyEditor allFiles={allFiles} setAllFiles={setAllFiles} currentNode={currentFile} />
      <Canvas htmlOutput={`<div className='text-lg'><h1>Hello world!</h1><p>This is your rendered DSL.</p></div>`} />
    </div>
  )
}
