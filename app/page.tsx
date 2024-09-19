import { TutorialLayout } from "@/components/tutorial-layout"
import { getAvailableLanguages, getTutorialStructure, getTutorialContent, Tutorial } from "@/lib/tutorials"

function findFirstTutorial(tutorials: Tutorial[]): Tutorial | null {
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

export default async function Home(): Promise<JSX.Element> {
  try {
    const languages = await getAvailableLanguages()
  
    if (languages.length === 0) {
      return <div>No tutorial content available. Please add some tutorials.</div>
    }

    const initialLanguage = languages[0]
    const initialTutorials = await getTutorialStructure(initialLanguage)
  
    let initialContent = 'No tutorial content available.'
    const firstTutorial = findFirstTutorial(initialTutorials)
    if (firstTutorial) {
      initialContent = await getTutorialContent(initialLanguage, firstTutorial.path)
    }

    return (
      <main>
        <TutorialLayout
          initialLanguages={languages}
          initialTutorials={initialTutorials}
          initialContent={initialContent}
        />
      </main>
    )
  } catch (error) {
    console.error('Error in Home component:', error)
    return <div>An error occurred while loading the content. Please try again later.</div>
  }
}
