import { getTutorialStructure } from "@/lib/tutorials"
import Link from 'next/link'

export default async function LanguageLayout({ 
  children,
  params 
}: { 
  children: React.ReactNode,
  params: { language: string }
}) {
  const tutorials = await getTutorialStructure(params.language)

  return (
    <div className="flex">
      <nav className="w-64 bg-gray-200 p-4">
        <h2 className="text-xl font-bold mb-4">{decodeURIComponent(params.language)} Tutorials</h2>
        <ul>
          {tutorials.map(tutorial => (
            <li key={tutorial.path}>
              <Link href={`/${params.language}/${tutorial.path}`} className="text-blue-500 hover:underline">
                {tutorial.title}
              </Link>
              {tutorial.items && (
                <ul className="ml-4">
                  {tutorial.items.map(subItem => (
                    <li key={subItem.path}>
                      <Link href={`/${params.language}/${subItem.path}`} className="text-blue-500 hover:underline">
                        {subItem.title}
                      </Link>
                    </li>
                  ))}
                </ul>
              )}
            </li>
          ))}
        </ul>
      </nav>
      <main className="flex-1 p-4">
        {children}
      </main>
    </div>
  )
}