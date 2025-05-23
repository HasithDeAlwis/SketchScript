import { axiosInstance } from '../shared/axiosInstance';
import { File } from '../models/file';

export interface FileTreeNode extends File {
  id: string;
  name: string;
  parent: string | null;
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

export async function updateFileMetadata(node: File): Promise<FileTreeNode> {
  const updated = {
    updateReqFileId: node.file_id,
    updateReqProjectId: node.file_project_id,
    updateReqName: node.file_name,
  };
  const res = await axiosInstance.put(`/file`, updated, {
    withCredentials: true,
  });
  if (res.status !== 200) {
    throw new Error('Failed to update file metadata');
  }
  return {
    ...res.data,
    id: res.data.file_id,
    name: res.data.file_name,
    parent: res.data.file_parent_folder_id,
    // children: res.data.file_type === 'file' ? undefined : [],
  }
}

export async function deleteFileMetadata(fileId: string): Promise<void> {
  const res = await axiosInstance.delete(`/file/${fileId}`, {
    withCredentials: true,
    validateStatus: () => true,
  });
  if (res.status !== 200) {
    throw new Error('Failed to delete file metadata');
  }
  return
}

function buildFileTree(files: File[]): FileTreeNode[] {
  // TODO: Reimplememnt Folder structure
  // const lookup: Record<string, FileTreeNode[]> = {};
  return files.map(file => ({
    ...file,
    id: file.file_id,
    name: file.file_name,
    parent: file.file_parent_folder_id,
    children: undefined, //file.file_type === 'file' ? undefined : [],
  }));

  // for (const file of files) {
  //   const node: FileTreeNode = {
  //     ...file,
  //     id: file.file_id,
  //     name: file.file_name,
  //     parent: file.file_parent_folder_id,
  //     children: undefined //file.file_type === 'file' ? undefined : [],
  //   };

  //   // const parentId = file.file_parent_folder_id;

  //   // if (parentId) {
  //   //   if (!lookup[parentId]) lookup[parentId] = [];
  //   //   lookup[parentId].push(node);
  //   // } else {
  //   //   root.push(node);
  //   // }
  // }

  // const allNodes = [...root, ...Object.values(lookup).flat()];
  // const byId = new Map(allNodes.map(n => [n.file_id, n]));

  // for (const node of allNodes) {
  //   const parentId = node.file_parent_folder_id;
  //   if (parentId && byId.has(parentId)) {
  //     const parent = byId.get(parentId)!;
  //     if (!parent.children) parent.children = [];
  //     parent.children.push(node);
  //   }
  // }
  // console.log('File tree:', root);
}
