import DOMPurify from 'isomorphic-dompurify'

type CanvasProps = {
  htmlOutput: string;
};
export function Canvas({ htmlOutput }: CanvasProps) {
  const safeHtml = DOMPurify.sanitize(htmlOutput);
  return (
    <div className="w-1/2 p-4 overflow-auto text-black bg-white">
      <div dangerouslySetInnerHTML={{ __html: safeHtml }} />
    </div>
  )
}
