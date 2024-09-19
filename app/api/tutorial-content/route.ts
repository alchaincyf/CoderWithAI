import { NextResponse } from 'next/server'
import fs from 'fs'
import path from 'path'
import matter from 'gray-matter'

const tutorialsDirectory = path.join(process.cwd(), 'tutorials')

export async function GET(request: Request) {
  const { searchParams } = new URL(request.url)
  const language = searchParams.get('language')
  const tutorialPath = searchParams.get('path')

  if (!language || !tutorialPath) {
    return NextResponse.json({ error: 'Invalid parameters' }, { status: 400 })
  }

  try {
    const decodedLanguage = decodeURIComponent(language)
    const decodedPath = decodeURIComponent(tutorialPath)
    const fullPath = path.join(tutorialsDirectory, decodedLanguage, `${decodedPath}.md`)
    
    console.log('Attempting to read file:', fullPath)
    
    if (!fs.existsSync(fullPath)) {
      console.error('File not found:', fullPath)
      return NextResponse.json({ error: 'Tutorial not found' }, { status: 404 })
    }
    
    const fileContents = fs.readFileSync(fullPath, 'utf8')
    const { content } = matter(fileContents)
    return new NextResponse(content)
  } catch (error) {
    console.error(`Error reading tutorial content: ${error}`)
    return NextResponse.json({ error: 'Error loading tutorial content' }, { status: 500 })
  }
}