import { LanguageProvider } from './LanguageProvider'
import ReactMarkdown from 'react-markdown'

function findFirstTutorial(tutorials: any[]): any {
  for (const item of tutorials) {
    if (item.items) {
      const found = findFirstTutorial(item.items)
      if (found) return found
    } else {
      return item
    }
  }
  return null
}

export default async function Page({ params }: { params: { language: string } }) {
  const { tutorials } = await LanguageProvider({ language: params.language })
  const firstTutorial = findFirstTutorial(tutorials)
  
  let content = 'No tutorial content available.'
  if (firstTutorial) {
    const encodedLanguage = encodeURIComponent(params.language)
    const encodedPath = encodeURIComponent(firstTutorial.path)
    const url = new URL('/api/tutorial-content', 'http://localhost:3000')
    url.searchParams.set('language', encodedLanguage)
    url.searchParams.set('path', encodedPath)
    
    try {
      const res = await fetch(url.toString(), { cache: 'no-store' })
      if (res.ok) {
        content = await res.text()
      } else {
        console.error('Failed to fetch tutorial content:', await res.text())
        content = 'Failed to load tutorial content.'
      }
    } catch (error) {
      console.error('Error fetching tutorial content:', error)
      content = 'Error loading tutorial content.'
    }
  }

  return (
    <div className="prose max-w-none">
      <h1 className="text-3xl font-bold mb-4">{decodeURIComponent(params.language)} Tutorials</h1>
      <ReactMarkdown>{content}</ReactMarkdown>
    </div>
  )
}