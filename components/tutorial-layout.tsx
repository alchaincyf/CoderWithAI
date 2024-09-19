'use client'

import React, { useEffect, useState } from 'react'
import Link from 'next/link'
import { usePathname } from 'next/navigation'
import { Book, ChevronDown } from "lucide-react"
import { ScrollArea } from "@/components/ui/scroll-area"
import { Button } from "@/components/ui/button"
import { DropdownMenu, DropdownMenuContent, DropdownMenuItem, DropdownMenuTrigger } from "@/components/ui/dropdown-menu"

export default function TutorialLayout({ children, _language }: { children: React.ReactNode, _language: string }) {
  const pathname = usePathname()
  const [, language] = pathname ? pathname.split('/') : []
  const [mounted, setMounted] = useState(false)
  const [visibleLanguages, setVisibleLanguages] = useState(languages)
  const [moreLanguages, setMoreLanguages] = useState<string[]>([])

  useEffect(() => {
    setMounted(true)
    const handleResize = () => {
      const containerWidth = window.innerWidth - 300 // Approximate space for logo and padding
      const buttonWidth = 100 // Approximate width of each language button
      const visibleCount = Math.floor(containerWidth / buttonWidth)
      setVisibleLanguages(languages.slice(0, visibleCount - 1)) // -1 to account for "More" button
      setMoreLanguages(languages.slice(visibleCount - 1))
    }

    handleResize()
    window.addEventListener('resize', handleResize)
    return () => window.removeEventListener('resize', handleResize)
  }, [languages])

  if (!mounted) {
    return null
  }

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
            {visibleLanguages.map((lang) => (
              <Link key={lang} href={`/${lang}`}>
                <Button variant="ghost" className="rounded-none">
                  {decodeURIComponent(lang)}
                </Button>
              </Link>
            ))}
            {moreLanguages.length > 0 && (
              <DropdownMenu>
                <DropdownMenuTrigger asChild>
                  <Button variant="ghost" className="rounded-none">
                    More <ChevronDown className="ml-1 h-4 w-4" />
                  </Button>
                </DropdownMenuTrigger>
                <DropdownMenuContent>
                  {moreLanguages.map((lang) => (
                    <DropdownMenuItem key={lang}>
                      <Link href={`/${lang}`}>{decodeURIComponent(lang)}</Link>
                    </DropdownMenuItem>
                  ))}
                </DropdownMenuContent>
              </DropdownMenu>
            )}
          </div>
        </ScrollArea>
      </header>
      <div className="flex-1">
        {children}
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