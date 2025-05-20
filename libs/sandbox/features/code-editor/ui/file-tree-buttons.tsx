import { Button } from '@shared/ui/button';
import { FilePlus2, FolderPlus } from 'lucide-react';
import type { TreeApi } from 'react-arborist';
import type { RefObject } from 'react';
import type { FileData } from './file-tree'; // Or wherever you defined it

type FileTreeButtonsProps = {
  treeRef: RefObject<TreeApi<FileData> | null>;
};

export function FileTreeButtons({ treeRef }: FileTreeButtonsProps) {
  const rootId = treeRef.current?.root.id;

  return (
    <div className="flex grid grid-cols-2 gap-2 p-2">
      <Button
        className="w-full"
        onClick={() => {
          if (rootId) treeRef.current?.createLeaf();
        }}
      >
        <FilePlus2 />
      </Button>
      <Button
        className="w-full"
        onClick={() => {
          if (rootId) treeRef.current?.createInternal();
        }}
      >
        <FolderPlus />
      </Button>
    </div>
  );
}
