import { NextResponse } from 'next/server'
import fs from 'fs'
import path from 'path'
import matter from 'gray-matter'

const tutorialsDirectory = path.join(process.cwd(), 'tutorials')

function getDirectoryStructure(dirPath: string, basePath: string) {
  const items = fs.readdirSync(dirPath, { withFileTypes: true })
  const structure = items.map(item => {
    const itemPath = path.join(dirPath, item.name)
    if (item.isDirectory()) {
      return {
        title: item.name,
        path: path.relative(path.join(tutorialsDirectory, basePath), itemPath),
        items: getDirectoryStructure(itemPath, basePath)
      }
    } else if (item.isFile() && item.name.endsWith('.md')) {
      try {
        const fileContents = fs.readFileSync(itemPath, 'utf8')
        const { data } = matter(fileContents)
        return {
          title: data.title || item.name.replace('.md', ''),
          path: path.relative(path.join(tutorialsDirectory, basePath), itemPath).replace('.md', '')
        }
      } catch (error) {
        console.error(`Error parsing file ${itemPath}:`, error)
        return {
          title: item.name.replace('.md', ''),
          path: path.relative(path.join(tutorialsDirectory, basePath), itemPath).replace('.md', '')
        }
      }
    }
    return null
  }).filter(item => item !== null)

  return structure
}

export async function GET(request: Request) {
  const { searchParams } = new URL(request.url)
  const language = searchParams.get('language')

  if (!language) {
    return NextResponse.json({ error: 'Invalid language parameter' }, { status: 400 })
  }

  try {
    const languagePath = path.join(tutorialsDirectory, language)
    const structure = getDirectoryStructure(languagePath, language)
    return NextResponse.json(structure)
  } catch (error) {
    console.error(`Error getting tutorial structure: ${error}`)
    return NextResponse.json({ error: 'Error loading tutorial structure' }, { status: 500 })
  }
}