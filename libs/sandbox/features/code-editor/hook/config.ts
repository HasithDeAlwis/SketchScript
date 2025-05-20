import { type Monaco } from '@monaco-editor/react'
export const options = {
  fontSize: 14,
  fontFamily: 'Jetbrains-Mono',
  fontLigatures: true,
  wordWrap: 'on',
  minimap: {
    enabled: false
  },
  bracketPairColorization: {
    enabled: true
  },
  cursorBlinking: 'smooth',
  formatOnPaste: true,
  suggest: {
    showFields: false,
    showFunctions: false
  }
}

export function handleEditorDidMount(
  monaco: Monaco
) {
  const languageId = 'ui-script'

  monaco.editor.defineTheme('sketch-script-light', {
    base: 'vs',
    inherit: true,
    rules: [
      { token: '', foreground: '#2f2d35', background: '#fff9ec' },
      { token: 'tag.subject', foreground: '#ee5d9e' },     // Pink
      { token: 'number', foreground: '#3f3f46' },          // Slate-800
      { token: 'style', foreground: '#7c3aed' },           // Violet-600
      { token: 'string', foreground: '#22c55e' },          // Green-500
      { token: 'tag.delimiter', foreground: '#9ca3af' },   // Gray-400
      { token: 'text', foreground: '#2f2d35' },
    ],
    colors: {
      'editor.background': '#fff9ec',
      'editor.foreground': '#2f2d35',
      'editor.lineHighlightBackground': '#00000010',
      'editorCursor.foreground': '#3f3f46',
      'editorLineNumber.foreground': '#7d88a0',
      'editorLineNumber.activeForeground': '#ee5d9e',
      'editor.selectionBackground': '#00000014',
      'editor.inactiveSelectionBackground': '#00000008',
      'editorIndentGuide.background': '#00000008',
      'editorIndentGuide.activeBackground': '#7c3aed',
    }
  })

  monaco.editor.defineTheme('sketch-script-dark', {
    base: 'vs-dark',
    inherit: true,
    rules: [
      { token: '', foreground: '#d1d9e1', background: '#121a2f' },
      { token: 'tag.subject', foreground: '#f7b1cb' },     // Pink
      { token: 'number', foreground: '#d4d4d8' },          // Zinc-300
      { token: 'style', foreground: '#c084fc' },           // Violet-400
      { token: 'string', foreground: '#4ade80' },          // Green-400
      { token: 'tag.delimiter', foreground: '#94a3b8' },   // Slate-400
      { token: 'text', foreground: '#d1d9e1' },
    ],
    colors: {
      'editor.background': '#121a2f',
      'editor.foreground': '#d1d9e1',
      'editor.lineHighlightBackground': '#ffffff08',
      'editorCursor.foreground': '#c084fc',
      'editorLineNumber.foreground': '#b0b8d0',
      'editorLineNumber.activeForeground': '#f7b1cb',
      'editor.selectionBackground': '#ffffff14',
      'editor.inactiveSelectionBackground': '#ffffff08',
      'editorIndentGuide.background': '#ffffff05',
      'editorIndentGuide.activeBackground': '#c084fc',
    }
  })

  monaco.languages.register({ id: languageId })

  monaco.languages.setMonarchTokensProvider(languageId, {
    tokenizer: {
      root: [
        [/<(?=\s*\b(Box|button|picture|text)\b)/, 'tag.delimiter'],

        [/\b(Box|button|picture|text)\b/, 'tag.subject'],

        [/\b\d+\.\d+\b/, 'number'],

        [/(?:^|\s)(flex-[hv]|center|between|around|grid-col-\d+)(?=\s|>)/, 'style'],

        [/".*?"/, 'string'],

        [/>/, 'tag.delimiter'],

        [/[^<>"\d]+/, 'text'],
      ],
    }
  })

  monaco.languages.setLanguageConfiguration(languageId, {
    brackets: [],
    autoClosingPairs: [],
    surroundingPairs: [],
    onEnterRules: [
      {
        beforeText: /^\s*<Box\b[^>]*>$/,
        action: { indentAction: monaco.languages.IndentAction.Indent }
      }
    ]
  })
}
