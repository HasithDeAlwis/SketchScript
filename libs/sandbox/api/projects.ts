import { axiosInstance } from '../shared/axiosInstance';
import { createApi, fetchBaseQuery } from '@reduxjs/toolkit/query/react';
import { Project } from '../models/project';

export async function fetchProjectsByWorkspaceId(workspaceId: string): Promise<Project[]> {
  const res = await axiosInstance.get<Project[]>(`/project?wid=${workspaceId}`, {
    withCredentials: true,
    validateStatus: () => true,
  });

  if (res.status !== 200) {
    throw new Error(`project?wid=${workspaceId}`);
  }

  return res.data;
}


export const projectApi = createApi({
  reducerPath: 'projectApi',
  baseQuery: fetchBaseQuery({
    baseUrl: import.meta.env.VITE_PUBLIC_API_BASE,
    credentials: 'include',
  }),
  tagTypes: ['Project'],
  endpoints: (builder) => ({
    updateProject: builder.mutation<void, Project>({
      query: (project) => ({
        url: `project/${project.project_id}`,
        method: 'PUT',
        body: { name: project.project_name },
      }),
    }),
  }),
});

export const { useUpdateProjectMutation } = projectApi;