'use client'

import React from 'react'
import Link from 'next/link'
import { usePathname } from 'next/navigation'
import { Book } from "lucide-react"
import { ScrollArea } from "@/components/ui/scroll-area"
import { Button } from "@/components/ui/button"

export function TutorialLayout({ children, languages }: { children: React.ReactNode, languages: string[] }) {
  const pathname = usePathname()
  const [, language] = pathname ? pathname.split('/') : []

  return (
    <div className="flex flex-col min-h-screen">
      <header className="border-b">
        <div className="flex h-14 items-center px-4 lg:px-6">
          <Link className="flex items-center gap-2 font-semibold" href="/">
            <Book className="h-6 w-6" />
            <span>Programming Tutorials</span>
          </Link>
        </div>
        <ScrollArea className="w-full">
          <div className="flex border-t">
            {languages.map((lang) => (
              <Link key={lang} href={`/${lang}`}>
                <Button variant="ghost" className="rounded-none">
                  {decodeURIComponent(lang)}
                </Button>
              </Link>
            ))}
          </div>
        </ScrollArea>
      </header>
      <div className="flex flex-1">
        {language && (
          <nav className="w-64 bg-gray-100 p-4">
            {/* 这里将添加语言特定的侧边栏内容 */}
          </nav>
        )}
        <main className="flex-1 p-4">
          {children}
        </main>
      </div>
      <footer className="border-t p-4 text-center">
        <nav>
          <Link href="/" className="mx-2">Home</Link>
          <Link href="/about" className="mx-2">About</Link>
          <Link href="/contact" className="mx-2">Contact</Link>
        </nav>
      </footer>
    </div>
  )
}