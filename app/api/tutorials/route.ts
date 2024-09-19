import { NextResponse } from 'next/server'
import { getTutorialStructure, getTutorialContent } from '@/lib/tutorials'

export async function GET(request: Request): Promise<Response> {
  const { searchParams } = new URL(request.url)
  const language = searchParams.get('language')

  if (!language) {
    return NextResponse.json({ error: 'Language is required' }, { status: 400 })
  }

  const decodedLanguage = decodeURIComponent(language)
  const tutorials = await getTutorialStructure(decodedLanguage)
  let initialContent = 'Select a tutorial to begin.'

  if (tutorials.length > 0) {
    const firstTutorial = findFirstTutorial(tutorials)
    if (firstTutorial) {
      initialContent = await getTutorialContent(decodedLanguage, firstTutorial.path)
    }
  }

  return new Response(JSON.stringify({ tutorials, initialContent }), {
    headers: { 'Content-Type': 'application/json' },
  })
}

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