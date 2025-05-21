/* eslint-disable @typescript-eslint/no-unused-vars */
import { File } from '../../models/file';
import { axiosInstance } from '../../shared/axiosInstance';
import { FileTreeNode } from '../../api/files';

export function toFile(node: FileTreeNode): File {
    const {
        id,
        name,
        children,
        ...fileFields
    } = node;

    return fileFields;
}

export async function handleMove(nodes: FileTreeNode[], newParentId: string | null) {
    for (const node of nodes) {
        const updated = {
            ...toFile(node),
            file_parent_folder_id: newParentId,
            file_updated_at: new Date().toISOString(),
        };

        await axiosInstance.put(`/file/${node.file_id}`, updated);
    }
}


export async function handleRename(node: FileTreeNode, name: string) {
    const updated = {
        ...toFile(node),
        file_name: name,
        file_updated_at: new Date().toISOString(),
    };

    await axiosInstance.put(`/file/${node.file_id}`, updated);
}



async function handleCreate(parentId: string | null, index: number, projectId: string) {
    const now = new Date().toISOString();

    const newFile: FileTreeNode = {
        file_id: crypto.randomUUID(),
        file_project_id: projectId,
        file_parent_folder_id: parentId,
        file_name: 'untitled.txt',
        file_type: 'text',
        file_content: '',
        file_created_at: now,
        file_updated_at: now,
        id: '',
        name: '',
    };

    const file = toFile({ ...newFile, id: newFile.file_id, name: newFile.file_name });

    await axiosInstance.post(`/file/${file.file_project_id}`, file);

    return {
        ...newFile,
        id: newFile.file_id,
        name: newFile.file_name,
    };
}

async function handleDelete(nodes: FileTreeNode[]) {
    for (const node of nodes) {
        await axiosInstance.delete(`/file/${node.file_id}`);
    }
}
