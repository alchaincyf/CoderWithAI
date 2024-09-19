import { getTutorialContent } from "@/lib/tutorials"
import ReactMarkdown from 'react-markdown'

export default async function TutorialPage({ params }: { params: { language: string, tutorial: string } }) {
  console.log(`Fetching content for language: ${params.language}, tutorial: ${params.tutorial}`)
  const content = await getTutorialContent(params.language, params.tutorial)
  console.log(`Content fetched, length: ${content.length}`)

  if (content === 'Tutorial content not found.' || content === 'Error loading tutorial content.') {
    return <div className="text-red-500">{content}</div>
  }

  return (
    <div className="prose max-w-none">
      <ReactMarkdown>{content}</ReactMarkdown>
    </div>
  )
}