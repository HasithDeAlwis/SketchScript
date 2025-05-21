import { createSlice, PayloadAction } from '@reduxjs/toolkit';
import { User } from '../models/user'

type UserState = {
    data: User | null;
};

const initialState: UserState = {
    data: null,
};

const userSlice = createSlice({
    name: 'user',
    initialState,
    reducers: {
        setUser(state, action: PayloadAction<User>) {
            state.data = action.payload;
        },
        clearUser(state) {
            state.data = null;
        },
    },
});

export const { setUser, clearUser } = userSlice.actions;
export default userSlice.reducer;
