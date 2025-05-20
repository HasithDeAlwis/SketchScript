import { axiosInstance } from '@sandbox/shared'

export async function getFilesByProjectID(pid: string) {
  const response = await axiosInstance.get(`/project?wid=${pid}`)
  return response.data;
}
