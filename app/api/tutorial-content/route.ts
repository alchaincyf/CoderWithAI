import { NextResponse } from 'next/server'
import { getTutorialContent } from '@/lib/tutorials'

export async function GET(request: Request) {
  const { searchParams } = new URL(request.url)
  const language = searchParams.get('language')
  const tutorial = searchParams.get('tutorial')

  if (!language || !tutorial) {
    return NextResponse.json({ error: 'Language and tutorial are required' }, { status: 400 })
  }

  const decodedLanguage = decodeURIComponent(language)
  const decodedTutorial = decodeURIComponent(tutorial)
  const content = await getTutorialContent(decodedLanguage, decodedTutorial)

  return NextResponse.json({ content })
}