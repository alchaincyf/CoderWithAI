//ts-nocheck

'use client';

import { usePathname } from 'next/navigation'
import Link from 'next/link'
import { useState } from 'react'

interface TutorialItem {
  title: string;
  path: string;
  items?: TutorialItem[];
}

function RenderTutorialTree({ items, language, currentPath }: { items: TutorialItem[], language: string, currentPath: string }) {
  return (
    <ul className="space-y-2">
      {items.map((item) => (
        <li key={item.path} className="ml-4">
          <TutorialTreeItem item={item} language={language} currentPath={currentPath} />
        </li>
      ))}
    </ul>
  );
}

function TutorialTreeItem({ item, language, currentPath }: { item: TutorialItem, language: string, currentPath: string }) {
  const [isOpen, setIsOpen] = useState(false);
  const encodedPath = encodeURIComponent(item.path).replace(/%2F/g, '%2F');
  const isActive = currentPath === `/${language}/${encodedPath}`;

  // 如果是目录（有子项目）
  if (item.items && item.items.length > 0) {
    return (
      <div>
        <div 
          className="flex items-center cursor-pointer hover:bg-gray-100 p-2 rounded"
          onClick={() => setIsOpen(!isOpen)}
        >
          <span className="mr-2 text-gray-500">
            {isOpen ? '▼' : '▶'}
          </span>
          <span className="text-gray-700 hover:text-blue-600">
            {item.title}
          </span>
        </div>
        {isOpen && (
          <RenderTutorialTree items={item.items} language={language} currentPath={currentPath} />
        )}
      </div>
    );
  }

  // 如果是具体文章（没有子项目）
  return (
    <div>
      <div className="flex items-center">
        <Link 
          href={`/${encodeURIComponent(language)}/${encodedPath}`} 
          className={`p-2 hover:text-blue-600 ${isActive ? 'text-blue-600 font-bold' : ''}`}
        >
          {item.title}
        </Link>
      </div>
    </div>
  );
}

export default function ClientSideTutorialTree({ tutorials, language }: { tutorials: TutorialItem[], language: string }) {
  const currentPath = usePathname();
  return <RenderTutorialTree items={tutorials} language={language} currentPath={currentPath || ''} />;
}
