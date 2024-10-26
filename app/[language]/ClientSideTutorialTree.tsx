//ts-nocheck

'use client';

import { usePathname } from 'next/navigation'
import Link from 'next/link'

interface TutorialItem {
  title: string;
  path: string;
  items?: TutorialItem[];
}

function RenderTutorialTree({ items, language, currentPath }: { items: TutorialItem[], language: string, currentPath: string }) {
  return (
    <ul className="pl-4">
      {items.map((item) => {
        const itemPath = `/${encodeURIComponent(language)}/${encodeURIComponent(item.path)}`;
        const isActive = currentPath === itemPath;
        return (
          <li key={item.path} className="my-2">
            {item.items ? (
              <>
                <span className="font-semibold">{item.title}</span>
                <RenderTutorialTree items={item.items} language={language} currentPath={currentPath} />
              </>
            ) : (
              <Link 
                href={itemPath}
                className={`${isActive ? 'text-blue-600 font-bold' : 'text-gray-700'} hover:underline`}
              >
                {item.title}
              </Link>
            )}
          </li>
        );
      })}
    </ul>
  );
}

export default function ClientSideTutorialTree({ tutorials, language }: { tutorials: TutorialItem[], language: string }) {
  const currentPath = usePathname();
  return <RenderTutorialTree items={tutorials} language={language} currentPath={currentPath} />;
}