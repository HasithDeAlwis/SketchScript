import { useState, useRef } from 'react'
import { FileTree } from './file-tree'
import { Canvas } from '../canvas/ui'
import MyEditor from './editor'

export function Presenter() {
  const treeRef = useRef(null)

  const data = [
    { id: "1", name: "public", children: [{ id: "c1-1", name: "index.html" }] },
    { id: "2", name: "src", children: [
      { id: "c2-1", name: "App.js" },
      { id: "c2-2", name: "index.js" },
      { id: "c2-3", name: "styles.css" }
    ] },
    { id: "3", name: "package.json" },
    { id: "4", name: "README.md" }
  ]
  const [treeWidth, setTreeWidth] = useState(210)
  const [fileData, setFileData] = useState(data)


  return (
    <div className="flex p-2 h-screen max-w-screen overflow-hidden">
      <FileTree data={fileData} treeRef={treeRef} treeWidth={treeWidth} setTreeWidth={setTreeWidth} />
      <MyEditor treeWidth={treeWidth}/>
      <Canvas htmlOutput={`<div className='text-lg'><h1>Hello world!</h1><p>This is your rendered DSL.</p></div>`} />
    </div>
  )
}
