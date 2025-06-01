import axios from 'axios';

const isBrowser = typeof window !== 'undefined';

export const axiosInstance = axios.create({
  baseURL: isBrowser
    ? import.meta.env.VITE_PUBLIC_API_BASE
    : process.env.API_BASE,
  withCredentials: true,
  timeout: 10000,
});

export const axiosInstanceCompiler = axios.create({
  baseURL: isBrowser
    ? import.meta.env.VITE_PUBLIC_API_BASE_COMPILER
    : process.env.API_BASE_COMPILER,
});
