import { Button } from '@shared/ui/button'
import { FilePlus2, FolderPlus } from 'lucide-react'
export function FileTreeButtons({ treeRef }) {
  return (
    <div className='flex grid grid-cols-2 gap-2 p-2'>
      <Button className='w-full' onClick={() => treeRef.current.createLeaf(treeRef.current.root.id)}>
        <FilePlus2 />
      </Button>
      <Button className='w-full' onClick={() => treeRef.current.createInternal(treeRef.current.root.id)}>
        <FolderPlus />
      </Button>
    </div>
  )
}
