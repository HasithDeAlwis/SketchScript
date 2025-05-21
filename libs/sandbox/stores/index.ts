import { configureStore } from '@reduxjs/toolkit';
import userReducer from './userSlice';
import workspaceReducer from './workspaceSlice';
import themeReducer from './themeSlice';

export const store = configureStore({
    reducer: {
        user: userReducer,
        workspaces: workspaceReducer,
        theme: themeReducer,
    },
});

export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;
