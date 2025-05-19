import { PencilRuler, FolderClosed, FolderOpen, Pencil, Trash, Code } from 'lucide-react';

export const Node = ({ node, style, dragHandle, tree }) => {


  function getNodeIcon(node) {
    if (node.isLeaf && node.data.name.endsWith('.sks')) {
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
      style={{...style, width: '90%'}}
      className="group flex items-center justify-between hover:bg-stone-200 transition-colors cursor-pointer select-none "
      onClick={() => node.isInternal && node.toggle()}
    >
      {/* Left side: icon + name or input */}
      <div className="flex flex-1 gap-2 items-center justify-between max-w-full">
        <div className='flex flex-1 gap-2 items-center'>
          {getNodeIcon(node)}
          {node.isEditing ? (
            <input
              type="text"
              defaultValue={node.data.name}
              className="w-full text-sm font-medium text-primary border-none outline-none"
              onFocus={(e) => e.currentTarget.select()}
              onBlur={() => node.reset()}
              onKeyDown={(e) => {
                if (e.key === 'Escape') { node.reset(); e.preventDefault(); node.blur() };
                if (e.key === 'Enter') node.submit(e.currentTarget.value);
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
        <div className="w-full flex items-center gap-1 opacity-0 group-hover:opacity-100 transition-opacity">
          <div className='ml-auto'>
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
        </div>
      )}
      </div>

    </div>
  );
};
