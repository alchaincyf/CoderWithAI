import { getTutorialContent } from "@/lib/tutorials"
import ReactMarkdown from 'react-markdown'
import { ErrorBoundary } from 'react-error-boundary'

function ErrorFallback({error}: {error: Error}) {
  return (
    <div role="alert" className="text-red-500">
      <p>Error loading tutorial content:</p>
      <pre>{error.message}</pre>
    </div>
  )
}

async function TutorialContent({ language, tutorialPath }: { language: string, tutorialPath: string }) {
  console.log(`Fetching content for language: ${language}, tutorial: ${tutorialPath}`)
  
  const content = await getTutorialContent(language, tutorialPath)

  if (content === 'Tutorial content not found.' || content === 'Error loading tutorial content.') {
    throw new Error(content)
  }

  return (
    <div className="prose max-w-none">
      <ReactMarkdown>{content}</ReactMarkdown>
    </div>
  )
}

export default function TutorialPage({ params }: { params: { language: string, tutorial: string[] } }) {
  const language = decodeURIComponent(params.language)
  const tutorialPath = params.tutorial.map(decodeURIComponent).join('/')
  
  return (
    <ErrorBoundary FallbackComponent={ErrorFallback}>
      <TutorialContent language={language} tutorialPath={tutorialPath} />
    </ErrorBoundary>
  )
}