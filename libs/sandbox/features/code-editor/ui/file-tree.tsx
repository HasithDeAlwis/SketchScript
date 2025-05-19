import { useEffect, useState, useRef } from 'react'
import { Tree } from 'react-arborist'
import { Cloud, EllipsisVertical } from 'lucide-react'
import { Node } from './tree-node.tsx'
import { FileTreeButtons } from './file-tree-buttons'

export function FileTree({ data, treeRef }) {
  const [treeWidth, setTreeWidth] = useState(210)
  const isResizing = useRef(false)

  const handleMouseDown = () => {
    isResizing.current = true
  }

  const handleMouseUp = () => {
    isResizing.current = false
  }

  const handleMouseMove = (e: MouseEvent) => {
    if (isResizing.current) {
      setTreeWidth(Math.max(210, e.clientX))
    }
  }

  useEffect(() => {

    window.addEventListener('mousemove', handleMouseMove)
    window.addEventListener('mouseup', handleMouseUp)
    return () => {
      window.removeEventListener('mousemove', handleMouseMove)
      window.removeEventListener('mouseup', handleMouseUp)
    }
  }, [])

  return (
    <div className="flex border-r-2 border-primary h-screen bg-muted/20 pb-2 flex-col justify-between">
      {/* Main horizontal layout: resizable panel + handle */}
      <div className="flex flex-row h-full">
        {/* Resizable tree panel */}
        <div
          className="p-3 flex flex-col bg-muted/10 backdrop-blur-sm"
        >
          {/* Toolbar */}
          <div className="flex items-center justify-between border-border mb-4">
            <div className="flex items-center gap-2">
              <p className="text-sm font-medium">Files</p>
              <Cloud className="w-5 h-5" />
            </div>
            <EllipsisVertical className="w-4 h-4" />
          </div>

          {/* Tree view */}
          <div className="flex-1 overflow-auto"
          >
            <Tree ref={treeRef} initialData={data} width={treeWidth}>
              {Node}
            </Tree>
          </div>
        </div>

        {/* Drag handle */}
        <div
          onMouseDown={handleMouseDown}
          className="w-1 cursor-ew-resize bg-border hover:bg-border/50"
        />
      </div>

      <FileTreeButtons treeRef={treeRef} />
    </div>
  )
}
