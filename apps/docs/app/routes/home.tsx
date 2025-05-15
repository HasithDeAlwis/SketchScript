import type { Route } from './+types/home';
import { HomeLayout } from 'fumadocs-ui/layouts/home';
import { Link } from 'react-router';

export function meta({}: Route.MetaArgs) {
  return [
    { title: 'SketchScript' },
    { name: 'description', content: 'Official SketchScript docs' },
  ];
}

export default function Home() {
  return (
    <HomeLayout
      className="text-center"
      nav={{
        title: 'React Router',
      }}
    >
      <div className="py-12">
        <h1 className="text-xl font-bold mb-2">🛠️ SketchScript Docs</h1>
        <p className="text-fd-muted-foreground m-8">
          🚧 This docs site is a work in progress! Want to contribute? Check out our <Link to="/docs/contribution-guidelines" className="underline">contributing guidelines</Link> 📝 and help shape the future of SketchScript 💡
        </p>
        <Link
          className="text-sm bg-fd-primary text-fd-primary-foreground rounded-full font-medium px-4 py-2.5"
          to="/docs"
        >
          📖 Open Docs
        </Link>
      </div>
    </HomeLayout>
  );
}
