import DOMPurify from 'isomorphic-dompurify'

export function Canvas({ htmlOutput }) {
  const safeHtml = DOMPurify.sanitize(htmlOutput);
  return (
    <div className="w-1/2 p-4 overflow-auto bg-white text-black">
          <div dangerouslySetInnerHTML={{ __html: safeHtml }} />
    </div>
  )
}
