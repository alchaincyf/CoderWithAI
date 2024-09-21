import { getTutorialContent, getTutorialMetadata, getTutorialStructure, Tutorial } from "@/lib/tutorials"
import ReactMarkdown from 'react-markdown'
import { Metadata, ResolvingMetadata } from 'next'
import { ErrorBoundary } from 'react-error-boundary'
import Link from 'next/link'

type Props = {
  params: { language: string; tutorial: string[] }
  searchParams: { [key: string]: string | string[] | undefined }
}

export async function generateMetadata(
  { params }: Props,
  parent: ResolvingMetadata
): Promise<Metadata> {
  const language = params.language
  const tutorialPath = Array.isArray(params.tutorial) ? params.tutorial.join('/') : params.tutorial

  // 获取教程元数据
  const metadata = await getTutorialMetadata(language, tutorialPath)

  // 获取父级元数据
  const previousImages = (await parent).openGraph?.images || []

  const title = `${metadata.title} - ${language} ${metadata.category || ''} | CoderWithAI`

  return {
    title,
    description: metadata.description,
    keywords: metadata.keywords,
    openGraph: {
      title,
      description: metadata.description,
      url: `https://www.codewithai.com/${language}/${tutorialPath}`,
      siteName: 'CoderWithAI',
      images: [
        {
          url: metadata.image || 'https://www.codewithai.com/images/default-og-image.jpg',
          width: 1200,
          height: 630,
        },
        ...previousImages,
      ],
      locale: 'zh_CN',
      type: 'article',
    },
    twitter: {
      card: 'summary_large_image',
      title,
      description: metadata.description,
      images: [metadata.image || 'https://www.codewithai.com/images/default-twitter-image.jpg'],
    },
  }
}

function ErrorFallback({error}: {error: Error}) {
  return (
    <div role="alert" className="text-red-500">
      <p>Error loading tutorial content:</p>
      <pre>{error.message}</pre>
    </div>
  )
}

interface NavigationLink {
  title: string;
  path: string;
}

function findNavigationLinks(tutorials: Tutorial[], currentPath: string): { prev: NavigationLink | null, next: NavigationLink | null } {
  let prev: NavigationLink | null = null;
  let next: NavigationLink | null = null;
  let found = false;

  function traverse(items: Tutorial[]) {
    for (let i = 0; i < items.length; i++) {
      const item = items[i];
      if (found && !next && !item.items) {
        next = { title: item.title, path: item.path };
        return;
      }
      if (item.path === currentPath) {
        found = true;
        if (i > 0) {
          const prevItem = items[i - 1];
          prev = { title: prevItem.title, path: prevItem.path };
        }
      } else if (!found) {
        prev = { title: item.title, path: item.path };
      }
      if (item.items) {
        traverse(item.items);
      }
    }
  }

  traverse(tutorials);
  return { prev, next };
}

async function TutorialContent({ language, tutorialPath }: { language: string, tutorialPath: string }) {
  console.log(`Fetching content for language: ${language}, tutorial: ${tutorialPath}`)
  
  const content = await getTutorialContent(language, tutorialPath)
  const tutorials = await getTutorialStructure(language)
  const { prev, next } = findNavigationLinks(tutorials, tutorialPath)

  if (content === 'Tutorial content not found.' || content === 'Error loading tutorial content.') {
    throw new Error(content)
  }

  return (
    <div className="prose max-w-none">
      <ReactMarkdown>{content}</ReactMarkdown>
      <div className="mt-8 flex justify-between">
        {prev && (
          <Link href={`/${language}/${prev.path}`} className="text-blue-500 hover:underline">
            ← Previous: {prev.title}
          </Link>
        )}
        {next && (
          <Link href={`/${language}/${next.path}`} className="text-blue-500 hover:underline">
            Next: {next.title} →
          </Link>
        )}
      </div>
    </div>
  )
}

export default async function TutorialPage({ params }: Props) {
  const language = decodeURIComponent(params.language)
  const tutorialPath = params.tutorial.map(decodeURIComponent).join('/')
  
  return (
    <ErrorBoundary FallbackComponent={ErrorFallback}>
      <TutorialContent language={language} tutorialPath={tutorialPath} />
    </ErrorBoundary>
  )
}