import { redirect } from 'react-router';
import { axiosInstance } from '@sandbox/shared';
import { LoginCard } from '@sandbox/features/login/ui';

export async function clientLoader() {
  try {
    const res = await axiosInstance.get('/me', {
      withCredentials: true,
      validateStatus: () => true,
    });

    if (res.status === 200) {
      return redirect('/')
    }
  } catch (err) {

    console.error('Client loader error:', err);
  }
}

export default function Login() {
  return (
    <LoginCard />
  );
}
