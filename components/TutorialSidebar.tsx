'use client'

import { useState } from 'react'
import Link from 'next/link'
import { ChevronRight, ChevronDown } from 'lucide-react'
import { usePathname } from 'next/navigation'
import { Tutorial } from '@/lib/tutorials';

interface TutorialSidebarProps {
  tutorials: Tutorial[];
  language: string;
}

function TutorialItem({ item, language, depth = 0 }: { item: Tutorial, language: string, depth?: number }) {
  const [isOpen, setIsOpen] = useState(false)
  const hasItems = item.items && item.items.length > 0
  const pathname = usePathname()
  const isActive = pathname === `/${language}/${item.path}`

  const toggleOpen = (e: React.MouseEvent) => {
    if (hasItems) {
      e.preventDefault()
      setIsOpen(!isOpen)
    }
  }

  return (
    <li>
      <div className={`flex items-center py-2 ${isActive ? 'bg-blue-100' : ''}`}>
        <div style={{ width: `${depth * 16}px` }} />
        <Link 
          href={`/${encodeURIComponent(language)}/${encodeURIComponent(item.path)}`}
          onClick={toggleOpen}
          className={`flex-grow hover:text-blue-600 transition-colors duration-200 
            ${depth === 0 ? 'font-semibold' : 'text-sm text-gray-600 ml-2'} 
            ${isActive ? 'text-blue-600' : ''} 
            ${item.isOutline ? 'font-bold' : ''}`}
        >
          {item.title}
        </Link>
        {hasItems && (
          <button onClick={toggleOpen} className="focus:outline-none ml-2">
            {isOpen ? <ChevronDown size={16} /> : <ChevronRight size={16} />}
          </button>
        )}
      </div>
      {hasItems && isOpen && item.items && (
        <ul>
          {item.items.map((subItem: Tutorial) => (
            <TutorialItem key={subItem.path} item={subItem} language={language} depth={depth + 1} />
          ))}
        </ul>
      )}
    </li>
  )
}

export default function TutorialSidebar({ tutorials, language }: TutorialSidebarProps) {
  return (
    <nav className="w-64 bg-gray-50 p-4 overflow-auto h-[calc(100vh-8rem)] border-r border-gray-200">
      <h2 className="text-xl font-bold mb-4 text-gray-800">{language} Tutorials</h2>
      <ul>
        {tutorials.map(item => (
          <TutorialItem key={item.path} item={item} language={language} />
        ))}
      </ul>
    </nav>
  )
}
