import { FolderClosed, FolderOpen, Pencil, Trash, Code } from 'lucide-react';

export const Node = ({ node, style, dragHandle, tree }) => {
  return (
    <div
      ref={dragHandle}
      style={style}
      className="group flex items-center justify-between rounded-md hover:bg-zinc-300 transition-colors cursor-pointer select-none "
      onClick={() => node.isInternal && node.toggle()}
    >
      {/* Left side: icon + name or input */}
      <div className="flex items-center gap-2 flex-1">
        {node.isLeaf ? (
          <Code className="text-sky-400 w-4 h-4" />
        ) :
           node.isOpen ?
            (
          <FolderOpen
            className="w-4 h-4 text-yellow-400"
          />
            ) : <FolderClosed className='w-4 h-4 text-yellow-600'/>
          }

        {node.isEditing ? (
          <input
            type="text"
            defaultValue={node.data.name}
            className="w-full text-sm font-medium text-foreground bg-transparent border-none outline-none"
            onFocus={(e) => e.currentTarget.select()}
            onBlur={() => node.reset()}
            onKeyDown={(e) => {
              if (e.key === 'Escape') { node.reset(); e.preventDefault(); node.blur() };
              if (e.key === 'Enter') node.submit(e.currentTarget.value);
            }}
            autoFocus
          />
        ) : (
          <span className="text-sm font-medium text-foreground">
            {node.data.name}
          </span>
        )}
      </div>

      {!node.isEditing && (
        <div className="flex items-center gap-1 opacity-0 group-hover:opacity-100 transition-opacity">
          <button
            onClick={(e) => {
              e.stopPropagation();
              node.edit();
            }}
            title="Rename"
            className="cursor-pointer rounded hover:bg-muted/50 transition"
          >
            <Pencil className="w-4 h-4 text-foreground" />
          </button>
          <button
            onClick={(e) => {
              e.stopPropagation();
              tree.delete(node.id);
            }}
            title="Delete"
            className="cursor-pointer rounded hover:bg-muted/50 transition"
          >
            <Trash className="w-4 h-4 text-red-500" />
          </button>
        </div>
      )}
    </div>
  );
};
