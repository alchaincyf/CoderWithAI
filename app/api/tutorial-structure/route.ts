import { NextResponse } from 'next/server'
import fs from 'fs'
import path from 'path'
import matter from 'gray-matter'
import { Tutorial, getTutorialStructure } from '@/lib/tutorials'

const tutorialsDirectory = path.join(process.cwd(), 'tutorials')

function extractSortOrder(name: string): number {
  const match = name.match(/^(\d+)/)
  return match ? parseInt(match[1], 10) : Infinity
}

function getDirectoryStructure(dirPath: string, basePath: string): Tutorial[] {
  const items = fs.readdirSync(dirPath, { withFileTypes: true })
  const structure = items.map(item => {
    const itemPath = path.join(dirPath, item.name)
    const relativePath = path.relative(path.join(tutorialsDirectory, basePath), itemPath)
    const sortOrder = extractSortOrder(item.name)
    const isOutline = !relativePath.includes(path.sep) || item.name.toLowerCase().includes('大纲')

    if (item.isDirectory()) {
      return {
        title: item.name,
        path: relativePath,
        items: getDirectoryStructure(itemPath, basePath),
        sortOrder,
        isOutline: false
      }
    } else if (item.isFile() && item.name.endsWith('.md')) {
      try {
        const fileContents = fs.readFileSync(itemPath, 'utf8')
        const { data } = matter(fileContents)
        return {
          title: data.title || item.name.replace('.md', ''),
          path: relativePath.replace('.md', ''),
          isOutline: isOutline,
          sortOrder: isOutline ? -1 : sortOrder,
          items: item.isDirectory() ? getDirectoryStructure(itemPath, basePath) : undefined
        } as Tutorial;
      } catch (error) {
        console.error(`Error parsing file ${itemPath}:`, error)
        return {
          title: item.name.replace('.md', ''),
          path: relativePath.replace('.md', ''),
          sortOrder,
          isOutline: false
        }
      }
    }
    return null
  }).filter((item): item is Tutorial => item !== null)

  structure.sort((a, b) => {
    if (a.isOutline && !b.isOutline) return -1
    if (!a.isOutline && b.isOutline) return 1
    return a.sortOrder - b.sortOrder
  })

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