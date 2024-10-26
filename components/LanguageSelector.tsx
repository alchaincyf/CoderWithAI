'use client';

import React, { useState } from 'react'
import Link from 'next/link'
import { usePathname } from 'next/navigation'
import { ChevronDownIcon } from '@heroicons/react/20/solid'

export default function LanguageSelector({ languages }: { languages: string[] }) {
  const [isOpen, setIsOpen] = useState(false);
  const pathname = usePathname()
  const currentLanguage = pathname ? pathname.split('/')[1] : 'Select Language'

  return (
    <div className="relative">
      <button
        onClick={() => setIsOpen(!isOpen)}
        className="flex items-center space-x-2 px-4 py-2 bg-white border border-gray-300 rounded-md shadow-sm text-sm font-medium text-gray-700 hover:bg-gray-50 focus:outline-none focus:ring-2 focus:ring-offset-2 focus:ring-indigo-500"
      >
        <span>{currentLanguage}</span>
        <ChevronDownIcon className="h-5 w-5 text-gray-400" aria-hidden="true" />
      </button>

      {isOpen && (
        <div className="absolute z-10 mt-1 w-56 rounded-md shadow-lg bg-white ring-1 ring-black ring-opacity-5">
          <div className="py-1" role="menu" aria-orientation="vertical" aria-labelledby="options-menu">
            {languages.map((lang) => (
              <Link
                key={lang}
                href={`/${lang}`}
                className="block px-4 py-2 text-sm text-gray-700 hover:bg-gray-100 hover:text-gray-900"
                role="menuitem"
                onClick={() => setIsOpen(false)}
              >
                {lang}
              </Link>
            ))}
          </div>
        </div>
      )}
    </div>
  )
}
