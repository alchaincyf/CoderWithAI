import Link from "next/link"
import { Button } from "@/components/ui/button"
import { getAvailableLanguages } from "@/lib/tutorials"

export default async function Home() {
  const languages = await getAvailableLanguages()

  return (
    <main className="flex-1 overflow-auto p-4 md:p-6">
      <h1 className="text-3xl font-bold mb-4">Welcome to Programming Tutorials</h1>
      <p className="mb-4">Select a programming language to get started with your learning journey.</p>
      <div className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {languages.map(lang => (
          <Link key={lang} href={`/${lang}`}>
            <Button
              variant="outline"
              className="h-auto py-4 justify-start w-full"
            >
              <span className="text-lg">{decodeURIComponent(lang)}</span>
            </Button>
          </Link>
        ))}
      </div>
    </main>
  )
}
