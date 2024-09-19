import { NextResponse } from 'next/server'
import { getTutorialStructure } from "@/lib/tutorials"

export async function GET(request: Request) {
  const { searchParams } = new URL(request.url)
  const language = searchParams.get('language')

  if (!language) {
    return NextResponse.json({ error: 'Language is required' }, { status: 400 })
  }

  const tutorials = await getTutorialStructure(language)
  return NextResponse.json({ tutorials, language: decodeURIComponent(language) })
}