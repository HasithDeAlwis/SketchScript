import { axiosInstance } from '@sandbox/shared'

export function getFilesByProjectID(pid: string) {
  const response = await axiosInstance.get(`/file?pid=${pid}`)
  return response.data;
}
