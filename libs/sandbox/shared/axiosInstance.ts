import axios from 'axios';

const isBrowser = typeof window !== 'undefined';

export const axiosInstance = axios.create({
  baseURL: isBrowser
    ? import.meta.env.VITE_PUBLIC_API_BASE
    : process.env.API_BASE,
  withCredentials: true,
  timeout: 10000,
});

