import { createSlice, PayloadAction } from '@reduxjs/toolkit';

type ThemeState = 'light' | 'dark';

const initialState: ThemeState = 'light';

const themeSlice = createSlice({
    name: 'theme',
    initialState,
    reducers: {
        setTheme: (_, action: PayloadAction<ThemeState>) => action.payload,
        toggleTheme: (state) => (state === 'light' ? 'dark' : 'light'),
    },
});

export const { setTheme, toggleTheme } = themeSlice.actions;
export default themeSlice.reducer;
