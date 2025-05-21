import { axiosInstance } from '../shared/axiosInstance';
import { File } from '../models/file';

export interface FileTreeNode extends File {
  id: string;        // same as file_id
  name: string;      // same as file_name
  children?: FileTreeNode[];
}

export async function fetchFilesByProjectId(projectId: string): Promise<FileTreeNode[]> {
  const res = await axiosInstance.get<File[]>(`/file?pid=${projectId}`, {
    withCredentials: true,
    validateStatus: () => true,
  });

  if (res.status !== 200) {
    throw new Error('Failed to fetch files');
  }

  return buildFileTree(res.data);
}

function buildFileTree(files: File[]): FileTreeNode[] {
  const lookup: Record<string, FileTreeNode[]> = {};
  const root: FileTreeNode[] = [];

  for (const file of files) {
    const node: FileTreeNode = {
      ...file,
      id: file.file_id,
      name: file.file_name,
    };

    const parentId = file.file_parent_folder_id;

    if (parentId) {
      if (!lookup[parentId]) lookup[parentId] = [];
      lookup[parentId].push(node);
    } else {
      root.push(node);
    }
  }

  const allNodes = [...root, ...Object.values(lookup).flat()];
  const byId = new Map(allNodes.map(n => [n.file_id, n]));

  for (const node of allNodes) {
    const parentId = node.file_parent_folder_id;
    if (parentId && byId.has(parentId)) {
      const parent = byId.get(parentId)!;
      if (!parent.children) parent.children = [];
      parent.children.push(node);
    }
  }

  return root;
}
