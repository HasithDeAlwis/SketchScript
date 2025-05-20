import {
  PencilRuler,
  FolderClosed,
  FolderOpen,
  Pencil,
  Trash,
  Code,
} from "lucide-react";
import type { TreeApi, NodeApi } from "react-arborist";

type FileData = {
  name: string;
};

type NodeProps = {
  node: NodeApi<FileData>;
  style: React.CSSProperties;
  dragHandle?: React.Ref<HTMLDivElement>;
  tree: TreeApi<FileData>;
};

export const Node: React.FC<NodeProps> = ({ node, style, dragHandle, tree }) => {
  function getNodeIcon(node: NodeApi<FileData>) {
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
      ref={dragHandle}
      style={{ ...style, width: "90%" }}
      className="flex items-center justify-between transition-colors cursor-pointer select-none group hover:bg-stone-200"
      onClick={() => node.isInternal && node.toggle()}
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
              onBlur={() => node.reset()}
              onKeyDown={(e) => {
                if (e.key === "Escape") {
                  node.reset();
                  e.preventDefault();
                  e.stopPropagation();
                }
                if (e.key === "Enter") {
                  node.submit(e.currentTarget.value);
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
          <div className="flex items-center w-full gap-1 transition-opacity opacity-0 group-hover:opacity-100">
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
                onClick={(e) => {
                  e.stopPropagation();
                  tree.delete(node.id);
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
