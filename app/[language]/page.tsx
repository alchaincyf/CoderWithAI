import { getTutorialStructure, getTutorialContent } from "@/lib/tutorials"
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
  const tutorials = await getTutorialStructure(params.language)
  const firstTutorial = findFirstTutorial(tutorials)
  
  let content = 'No tutorial content available.'
  if (firstTutorial) {
    content = await getTutorialContent(params.language, firstTutorial.path)
  }

  return (
    <div className="prose max-w-none">
      <h1 className="text-3xl font-bold mb-4">{decodeURIComponent(params.language)} Tutorials</h1>
      <ReactMarkdown>{content}</ReactMarkdown>
    </div>
  )
}