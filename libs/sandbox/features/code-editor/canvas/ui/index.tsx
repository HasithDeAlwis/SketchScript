import { useState } from 'react';
import { pollDownloadFile } from '../../../../api/files';

type CanvasProps = {
  currentFile: string;
};
export function Canvas({ currentFile }: CanvasProps) {

  const [htmlOutput, setHtmlOutput] = useState('Loading...');
  pollDownloadFile(currentFile, setHtmlOutput)

  // const sampleHtml = '<div class="p-4 bg-blue-100 text-blue-900 rounded shadow">Hello from Tailwind-styled HTML!</div>';

  return (
    <div className="w-1/2 p-4 overflow-auto text-black bg-white">
      <div dangerouslySetInnerHTML={{ __html: htmlOutput }} />
    </div>
  )
}
