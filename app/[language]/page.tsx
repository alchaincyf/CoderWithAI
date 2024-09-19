import { LanguageProvider } from './LanguageProvider'
import Link from 'next/link'

interface TutorialItem {
  title: string;
  path: string;
  items?: TutorialItem[];
}

function renderTutorialTree(items: TutorialItem[], language: string, depth = 0) {
  return (
    <ul className={`pl-${depth * 4}`}>
      {items.map((item) => (
        <li key={item.path} className="my-2">
          {item.items ? (
            <>
              <span className="font-semibold">{item.title}</span>
              {renderTutorialTree(item.items, language, depth + 1)}
            </>
          ) : (
            <Link 
              href={`/${encodeURIComponent(language)}/${encodeURIComponent(item.path)}`} 
              className="text-blue-600 hover:underline"
            >
              {item.title}
            </Link>
          )}
        </li>
      ))}
    </ul>
  )
}

export default async function Page({ params }: { params: { language: string } }) {
  const { tutorials, language } = await LanguageProvider({ language: params.language })

  return (
    <div className="p-4">
      <h1 className="text-3xl font-bold mb-4">{decodeURIComponent(language)} Tutorials</h1>
      <div className="bg-white shadow-md rounded p-6">
        {renderTutorialTree(tutorials, language)}
      </div>
    </div>
  )
}