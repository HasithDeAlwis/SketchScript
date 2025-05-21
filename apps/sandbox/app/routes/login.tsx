import { redirect } from 'react-router';
import { axiosInstance } from '@sandbox/shared';
import { LoginCard } from '@sandbox/features/login/ui';
import { User } from '@sandbox/models/user';
import { Workspace } from '@sandbox/models/workspace';
import { store } from '@sandbox/stores';
import { setUser } from '@sandbox/stores/userSlice';
import { setWorkspaces } from '@sandbox/stores/workspaceSlice';

type MeResponse = {
  user: User;
  workspaces: Workspace[];
};

export async function clientLoader() {
  try {
    const res = await axiosInstance.get<MeResponse>('/me', {
      withCredentials: true,
      validateStatus: () => true,
    });

    if (res.status === 401 || res.status === 403) {
      return null;
    }

    const { user, workspaces } = res.data;

    if (user && Array.isArray(workspaces)) {
      store.dispatch(setUser(user));
      store.dispatch(
        setWorkspaces({
          allWorkspaces: workspaces,
          currentWorkspace: workspaces[0] ?? null,
        })
      );
    }

    if (res.status === 200) {
      return redirect('/')
    }


  } catch (err) {
    console.error('Client loader error:', err);
    return null;
  }
}

export default function Login() {
  return (
    <LoginCard />
  );
}
