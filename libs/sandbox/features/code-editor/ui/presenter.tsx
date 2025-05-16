import { FileTree } from './file-tree'
import { MyEditor } from './editor'

export function Presenter() {
  const data = [
    {
      id: "1",
      name: "public",
      children: [{ id: "c1-1", name: "index.html" }]
    },
    {
      id: "2",
      name: "src",
      children: [
        { id: "c2-1", name: "App.js" },
        { id: "c2-2", name: "index.js" },
        { id: "c2-3", name: "styles.css" }
      ]
    },
    { id: "3", name: "package.json" },
    { id: "4", name: "README.md" }
  ];
  return (
    <>
    <div className='flex px-3'>
    Hello, world
    </div>
    <div className='flex justify-between'>
    <FileTree data={data} />
    <MyEditor/>
    </div>
    </>
  )
}
