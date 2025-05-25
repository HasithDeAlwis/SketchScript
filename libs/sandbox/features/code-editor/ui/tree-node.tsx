import { FileTreeNode, updateFileMetadata, deleteFileMetadata } from '../../../api/files';
import { File } from '../../../models/file';

import {
  PencilRuler,
  FolderClosed,
  FolderOpen,
  Pencil,
  Trash,
  Code,
} from "lucide-react";
import type { NodeApi, NodeRendererProps } from "react-arborist";

type CustomNodeProps = NodeRendererProps<FileTreeNode> & {
  setData: React.Dispatch<React.SetStateAction<FileTreeNode[]>>;
  setCurrentFile: React.Dispatch<React.SetStateAction<string>>;
};

export const Node: React.FC<CustomNodeProps> = ({ node, style, tree, setData, setCurrentFile }) => {
  function getNodeIcon(node: NodeApi<FileTreeNode>) {
    if (node.isLeaf && node.data.name.endsWith(".sks")) {
      return <PencilRuler className="w-4 h-4 text-primary" />;
    } else if (node.isLeaf) {
      return <Code className="w-4 h-4 text-primary" />;
    } else if (node.isOpen) {
      return <FolderOpen className="w-4 h-4 text-primary" />;
    } else {
      return <FolderClosed className="w-4 h-4 text-primary" />;
    }
  }

  return (
    <div
      style={{ ...style, width: "90%" }}
      className="flex items-center justify-between transition-colors cursor-pointer select-none group hover:bg-stone-200"
      onClick={() => {
        if (node.isLeaf) {
          setCurrentFile(node.id);
        }
        return node.isInternal && node.toggle()
      }}
    >
      <div className="flex items-center justify-between flex-1 max-w-full gap-2">
        <div className="flex items-center flex-1 gap-2">
          {getNodeIcon(node)}
          {node.isEditing ? (
            <input
              type="text"
              defaultValue={node.data.name}
              className="w-full text-sm font-medium border-none outline-none text-primary"
              onFocus={(e) => e.currentTarget.select()}
              onKeyDown={async (e) => {
                if (e.key === "Escape") {
                  node.reset();
                  e.preventDefault();
                  e.stopPropagation();
                }
                if (e.key === "Enter") {
                  const newName = e.currentTarget.value
                  const { id: _id, parent: _parent, children: _children, name: _name, ...rest } = node.data;
                  const toUpdate: File = {
                    ...rest,
                    file_name: newName,
                  };
                  try {
                    const res = await updateFileMetadata(toUpdate)
                    setData((prev) => {
                      const newData = [...prev];
                      const index = newData.findIndex((item) => item.id === node.id);
                      if (index !== -1) {
                        newData[index] = res;
                      }
                      return newData;
                    })

                  }
                  catch (error) {
                    console.error("Failed to update file metadata", error);
                  }
                }
              }}
              autoFocus
            />
          ) : (
            <span className="text-sm font-foreground text-foreground">
              {node.data.name}
            </span>
          )}
        </div>

        {!node.isEditing && (
          <div className="flex items-center gap-1 transition-opacity opacity-0 group-hover:opacity-100">
            <div className="flex items-center gap-1 ml-auto">
              <button
                onClick={(e) => {
                  e.stopPropagation();
                  node.edit();
                }}
                title="Rename"
                className="transition rounded cursor-pointer hover:bg-primary/10"
              >
                <Pencil className="w-4 h-4 text-foreground" />
              </button>
              <button
                onClick={async (e) => {
                  e.stopPropagation();
                  try {
                    await deleteFileMetadata(node.data.id);
                    setData((prev) => {
                      const newData = [...prev];
                      const index = newData.findIndex((item) => item.id === node.id);
                      if (index !== -1) {
                        newData.splice(index, 1);
                      }
                      return newData;
                    }
                    )
                  } catch (error) {
                    console.error("Failed to delete file metadata", error);
                  }
                }}
                title="Delete"
                className="transition rounded cursor-pointer hover:bg-primary/10"
              >
                <Trash className="w-4 h-4 text-red-500" />
              </button>
            </div>
          </div>
        )}
      </div>
    </div>
  );
};
