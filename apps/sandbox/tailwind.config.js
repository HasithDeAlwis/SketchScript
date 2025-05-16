// tailwind.config.js
import defaultTheme from 'tailwindcss/defaultTheme';

/** @type {import('tailwindcss').Config} */
export default {
  content: [
    '../sandbox/**/*',
    '../../libs/shared/**/*',
    '../../libs/sandbox/**/*'
  ],
  theme: {
    extend: {
      fontFamily: {
        mono: ['"JetBrains Mono"', ...defaultTheme.fontFamily.mono],
      },
      colors: {
        background: 'hsl(var(--background) / <alpha-value>)',
        foreground: 'hsl(var(--foreground) / <alpha-value>)',
        card: 'hsl(var(--card) / <alpha-value>)',
        'card-foreground': 'hsl(var(--card-foreground) / <alpha-value>)',
        popover: 'hsl(var(--popover) / <alpha-value>)',
        'popover-foreground': 'hsl(var(--popover-foreground) / <alpha-value>)',
        primary: 'hsl(var(--primary) / <alpha-value>)',
        'primary-foreground': 'hsl(var(--primary-foreground) / <alpha-value>)',
        secondary: 'hsl(var(--secondary) / <alpha-value>)',
        'secondary-foreground': 'hsl(var(--secondary-foreground) / <alpha-value>)',
        muted: 'hsl(var(--muted) / <alpha-value>)',
        'muted-foreground': 'hsl(var(--muted-foreground) / <alpha-value>)',
        accent: 'hsl(var(--accent) / <alpha-value>)',
        'accent-foreground': 'hsl(var(--accent-foreground) / <alpha-value>)',
        destructive: 'hsl(var(--destructive) / <alpha-value>)',
        'destructive-foreground': 'hsl(var(--destructive-foreground) / <alpha-value>)',
        border: 'hsl(var(--border) / <alpha-value>)',
        input: 'hsl(var(--input) / <alpha-value>)',
        ring: 'hsl(var(--ring) / <alpha-value>)',
        'color-1': 'hsl(var(--color-1) / <alpha-value>)',
        'color-2': 'hsl(var(--color-2) / <alpha-value>)',
        'color-3': 'hsl(var(--color-3) / <alpha-value>)',
        'color-4': 'hsl(var(--color-4) / <alpha-value>)',
        'color-5': 'hsl(var(--color-5) / <alpha-value>)',
      },
      borderRadius: {
        lg: 'var(--radius)',
        md: 'calc(var(--radius) - 2px)',
        sm: 'calc(var(--radius) - 4px)',
      },
      keyframes: {
        'accordion-down': {
          from: { height: '0' },
          to: { height: 'var(--radix-accordion-content-height)' },
        },
        'accordion-up': {
          from: { height: 'var(--radix-accordion-content-height)' },
          to: { height: '0' },
        },
        orbit: {
          '0%': {
            transform:
              'rotate(0deg) translateY(calc(var(--radius) * 1px)) rotate(0deg)',
          },
          '100%': {
            transform:
              'rotate(360deg) translateY(calc(var(--radius) * 1px)) rotate(-360deg)',
          },
        },
        rainbow: {
          '0%': { backgroundPosition: '0%' },
          '100%': { backgroundPosition: '200%' },
        },
      },
      animation: {
        'accordion-down': 'accordion-down 0.2s ease-out',
        'accordion-up': 'accordion-up 0.2s ease-out',
        orbit: 'orbit calc(var(--duration) * 1s) linear infinite',
        rainbow: 'rainbow var(--speed, 2s) infinite linear',
      },
      backgroundImage: {
        greendiant:
          'linear-gradient(200deg, var(--color-secondary) 10%, var(--color-primary) 90%)',
        'g-keyboardBlack':
          'linear-gradient(300deg, var(--color-g-keyboardblack-start) -10%, var(--color-background) 100%)',
        'g-nav-drawer-background':
          'linear-gradient(90deg, var(--color-background) 0%, var(--color-light-black) 25%, var(--color-light-black) 75%, var(--color-background) 100%)',
      },
      boxShadow: {
        dropShadow: '0px 4px 4px rgba(0, 0, 0, 0.25)',
        innerShadow: 'inset 0 4px 4px rgba(0, 0, 0, 0.25)',
        buttonKeyboard: `0.5px 0.5px 0px 0.6px rgba(0, 0, 0, 0.7),
          0.35px 0.35px 0.2px 0.75px rgba(255, 255, 255, 0.15) inset,
          4px 2px 4px -1px rgba(0, 0, 0, 0.25)`,
        buttonKeyboardHover: `0.25px 0.25px 0px 0.5px #0a0a0a,
          0.2px 0.2px 0.2px 0.35px rgba(255, 255, 255, 0.25) inset,
          0.2px 0.2px 0px 0.75px rgba(137, 237, 16, 0.25),
          0px 0px 10px -4px rgba(137, 237, 16, 0.6),
          4px 2px 4px -1px rgba(0, 0, 0, 0.25)`,
      },
    },
  },
  plugins: [],
};
