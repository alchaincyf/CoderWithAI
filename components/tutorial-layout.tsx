'use client'

import * as React from "react"
import Link from "next/link"
import { Book, ChevronDown, ChevronRight, Menu } from "lucide-react"
import ReactMarkdown from 'react-markdown'
import rehypeHighlight from 'rehype-highlight'
import rehypeRaw from 'rehype-raw'
import remarkGfm from 'remark-gfm'

import { Button } from "@/components/ui/button"
import {
  DropdownMenu,
  DropdownMenuContent,
  DropdownMenuItem,
  DropdownMenuTrigger,
} from "@/components/ui/dropdown-menu"
import {
  Collapsible,
  CollapsibleContent,
  CollapsibleTrigger,
} from "@/components/ui/collapsible"
import { ScrollArea } from "@/components/ui/scroll-area"
import { Sheet, SheetContent, SheetTrigger } from "@/components/ui/sheet"

interface TutorialItem {
  title: string;
  path: string;
  items?: TutorialItem[];
}

interface TutorialLayoutProps {
  initialLanguages: string[];
  initialTutorials: TutorialItem[];
  initialContent: string;
}

export function TutorialLayout({ initialLanguages, initialTutorials, initialContent }: TutorialLayoutProps) {
  const [languages] = React.useState<string[]>(initialLanguages)
  const [selectedLanguage, setSelectedLanguage] = React.useState(decodeURIComponent(initialLanguages[0]))
  const [visibleLanguages, setVisibleLanguages] = React.useState<string[]>([])
  const [moreLanguages, setMoreLanguages] = React.useState<string[]>([])
  const [tutorials, setTutorials] = React.useState<TutorialItem[]>(initialTutorials)
  const [tutorialContent, setTutorialContent] = React.useState(initialContent)

  React.useEffect(() => {
    const handleResize = () => {
      const containerWidth = window.innerWidth - 300 // Adjust this value as needed
      const buttonWidth = 120 // Adjust this value based on your button size
      const moreButtonWidth = 100 // Width of the "More" button
      const availableWidth = containerWidth - moreButtonWidth
      const visibleCount = Math.floor(availableWidth / buttonWidth)
      setVisibleLanguages(languages.slice(0, visibleCount))
      setMoreLanguages(languages.slice(visibleCount))
    }

    handleResize()
    window.addEventListener('resize', handleResize)
    return () => window.removeEventListener('resize', handleResize)
  }, [languages])

  const handleLanguageChange = async (lang: string) => {
    const decodedLang = decodeURIComponent(lang)
    setSelectedLanguage(decodedLang)
    const response = await fetch(`/api/tutorials?language=${lang}`)
    const data = await response.json()
    setTutorials(data.tutorials)
    setTutorialContent(data.initialContent)
  }

  const handleTutorialClick = async (tutorialPath: string) => {
    const encodedLang = encodeURIComponent(selectedLanguage)
    const encodedPath = encodeURIComponent(tutorialPath)
    const response = await fetch(`/api/tutorial-content?language=${encodedLang}&tutorial=${encodedPath}`)
    const data = await response.json()
    setTutorialContent(data.content)
  }

  const renderTutorialItems = (items: TutorialItem[]) => {
    return items.map((item, index) => {
      if (item.items) {
        return (
          <Collapsible key={index} className="mb-2">
            <CollapsibleTrigger className="flex w-full items-center justify-between py-2 text-sm font-medium">
              <span className="text-left pr-2">{item.title}</span>
              <ChevronRight className="h-4 w-4 flex-shrink-0" />
            </CollapsibleTrigger>
            <CollapsibleContent>
              <ul className="ml-4 space-y-1 py-2">
                {renderTutorialItems(item.items)}
              </ul>
            </CollapsibleContent>
          </Collapsible>
        )
      } else {
        return (
          <li key={index} className="flex items-center">
            <Button
              variant="ghost"
              className="w-full justify-start py-1 text-sm text-muted-foreground hover:text-foreground text-left"
              onClick={() => handleTutorialClick(item.path)}
            >
              <span className="line-clamp-2">{item.title}</span>
            </Button>
          </li>
        )
      }
    })
  }

  return (
    <div className="flex h-screen flex-col">
      <header className="border-b">
        <div className="flex h-14 items-center px-4 lg:px-6">
          <Link className="flex items-center gap-2 font-semibold" href="#">
            <Book className="h-6 w-6" />
            <span>Programming Tutorials</span>
          </Link>
          <Sheet>
            <SheetTrigger asChild>
              <Button variant="ghost" size="icon" className="ml-auto md:hidden">
                <Menu className="h-6 w-6" />
                <span className="sr-only">Toggle navigation menu</span>
              </Button>
            </SheetTrigger>
            <SheetContent side="left">
              <nav className="grid gap-2">
                {languages.map((lang) => (
                  <Button
                    key={lang}
                    variant={lang === selectedLanguage ? "default" : "ghost"}
                    className="justify-start"
                    onClick={() => handleLanguageChange(lang)}
                  >
                    {lang}
                  </Button>
                ))}
              </nav>
            </SheetContent>
          </Sheet>
        </div>
        <ScrollArea className="hidden w-full md:block">
          <div className="flex border-t">
            {visibleLanguages.map((lang) => (
              <Button
                key={lang}
                variant={decodeURIComponent(lang) === selectedLanguage ? "default" : "ghost"}
                className="rounded-none whitespace-nowrap"
                onClick={() => handleLanguageChange(lang)}
              >
                {decodeURIComponent(lang)}
              </Button>
            ))}
            {moreLanguages.length > 0 && (
              <DropdownMenu>
                <DropdownMenuTrigger asChild>
                  <Button variant="ghost" className="rounded-none">
                    More <ChevronDown className="ml-1 h-4 w-4" />
                  </Button>
                </DropdownMenuTrigger>
                <DropdownMenuContent align="end" className="max-h-[50vh] overflow-y-auto">
                  {moreLanguages.map((lang) => (
                    <DropdownMenuItem key={lang} onSelect={() => handleLanguageChange(lang)}>
                      {decodeURIComponent(lang)}
                    </DropdownMenuItem>
                  ))}
                </DropdownMenuContent>
              </DropdownMenu>
            )}
          </div>
        </ScrollArea>
      </header>
      <div className="flex-1 overflow-hidden">
        <div className="grid h-full md:grid-cols-[250px_1fr]">
          <div className="hidden border-r bg-muted/40 md:block overflow-hidden">
            <ScrollArea className="h-[calc(100vh-8rem)]">
              <nav className="p-4">
                <h2 className="mb-2 text-lg font-semibold">{selectedLanguage} Tutorials</h2>
                {renderTutorialItems(tutorials)}
              </nav>
            </ScrollArea>
          </div>
          <div className="flex-1 overflow-auto p-4 md:p-6">
            <div className="prose dark:prose-invert max-w-none">
              <ReactMarkdown
                rehypePlugins={[rehypeHighlight, rehypeRaw]}
                remarkPlugins={[remarkGfm]}
              >
                {tutorialContent}
              </ReactMarkdown>
            </div>
          </div>
        </div>
      </div>
      <footer className="border-t py-4 text-center text-sm text-muted-foreground">
        <p>
          Powered by{' '}
          <a
            href="https://www.bookai.top/"
            target="_blank"
            rel="noopener noreferrer"
            className="font-medium text-primary hover:underline"
          >
            BookAI.TOP
          </a>
          {' '}- Your Best AI Learning Website in Chinese
        </p>
        <p className="mt-2">
          <Link href="/about" className="hover:underline">About</Link>
          {' | '}
          <Link href="/contact" className="hover:underline">Contact</Link>
          {' | '}
          <Link href="/privacy" className="hover:underline">Privacy Policy</Link>
        </p>
      </footer>
    </div>
  )
}