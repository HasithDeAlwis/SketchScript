/* eslint-disable @typescript-eslint/no-unused-vars */
import { File } from '../../models/file';
import { axiosInstance } from '../../shared/axiosInstance';
import { FileTreeNode } from '../../api/files';
// import { NodeApi } from 'react-arborist';
import { Dispatch } from 'react';
// import { FileData } from './ui/file-tree';
// import { create } from 'domain';

export function toFile(node: FileTreeNode): File {
    const {
        id,
        name,
        children,
        ...fileFields
    } = node;

    return fileFields;
}

// export async function handleMove(dragIDs: string[], nodes: NodeApi[], newParentId: string | null, _index: number) {
//     for (const node of nodes) {
//         const updated = {
//             ...toFile(node),
//             file_parent_folder_id: newParentId,
//             file_updated_at: new Date().toISOString(),
//         };

//         await axiosInstance.put(`/file/${node.file_id}`, updated);
//     }

// }


export async function handleRename(node: FileTreeNode, name: string) {
    const updated = {
        ...toFile(node),
        file_name: name,
        file_updated_at: new Date().toISOString(),
    };

    await axiosInstance.put(`/file/${node.file_id}`, updated);
}



export async function handleCreate(parentId: string | null, type: string, projectId: string, setData: Dispatch<React.SetStateAction<FileTreeNode[]>>) {
    const newFileReqBody = {
        createReqProjectId: projectId,
        createReqParentId: parentId,
        createReqName: '',
        createReqFileType: type === 'leaf' ? 'file' : 'folder',
    };

    try {
        const res = await axiosInstance.post(`/file`, newFileReqBody);
        if (res.status !== 200) {
            throw new Error('Failed to create file');
        }

        const newFile = {
            ...res.data,
            name: res.data.file_name,
            id: res.data.file_id,
            parent: parentId,
            children: type === 'leaf' ? undefined : [],
        } as FileTreeNode


        setData((prev) => {
            return [...prev, newFile]
        });
        return newFile
    } catch (err) {
        console.error('Error creating file:', err);
    }
}

export async function handleDelete(nodes: FileTreeNode[]) {
    for (const node of nodes) {
        await axiosInstance.delete(`/file/${node.file_id}`);
    }
}
