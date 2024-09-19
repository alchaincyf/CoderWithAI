import { getAvailableLanguages } from "@/lib/tutorials"
import Link from 'next/link'

export default async function Home() {
  const languages = await getAvailableLanguages()

  return (
    <main className="flex-1 overflow-auto p-4 md:p-6">
      <h1 className="text-3xl font-bold mb-4">Welcome to CoderWithAI</h1>
      <p className="mb-4">Choose a programming language to get started:</p>
      <ul className="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
        {languages.map((lang) => (
          <li key={lang}>
            <Link href={`/${lang}`} className="block p-4 bg-white shadow rounded hover:shadow-md transition-shadow">
              {decodeURIComponent(lang)}
            </Link>
          </li>
        ))}
      </ul>
    </main>
  )
}
