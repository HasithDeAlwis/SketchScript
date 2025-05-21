import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { Workspace } from '../models/workspace';

type WorkspaceState = {
    allWorkspaces: Workspace[];
    currentWorkspace: Workspace | null;
};

const initialState: WorkspaceState = {
    allWorkspaces: [],
    currentWorkspace: null,
};

export const workspaceSlice = createSlice({
    name: 'workspaces',
    initialState,
    reducers: {
        setWorkspaces(
            _state,
            action: PayloadAction<{ allWorkspaces: Workspace[]; currentWorkspace: Workspace | null }>
        ) {
            return action.payload;
        },
        addWorkspace(state, action: PayloadAction<Workspace>) {
            state.allWorkspaces.push(action.payload);
        },
        clearWorkspaces() {
            return {
                allWorkspaces: [],
                currentWorkspace: null,
            };
        },
    },
});

export const { setWorkspaces, addWorkspace, clearWorkspaces } = workspaceSlice.actions;
export default workspaceSlice.reducer;
