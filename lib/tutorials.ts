import fs from 'fs'
import path from 'path'
import matter from 'gray-matter'
import { languageOrder } from '@/config/languageOrder'

const tutorialsDirectory = path.join(process.cwd(), 'tutorials')

export interface Tutorial {
  title: string;
  path: string;
  items?: Tutorial[];
}

export function getTutorialContent(language: string, tutorialPath: string) {
  try {
    const decodedLanguage = decodeURIComponent(language)
    const fullPath = path.join(tutorialsDirectory, decodedLanguage, `${tutorialPath}.md`)
    if (!fs.existsSync(fullPath)) {
      console.error(`File not found: ${fullPath}`)
      return 'Tutorial content not found.'
    }
    const fileContents = fs.readFileSync(fullPath, 'utf8')
    const { content } = matter(fileContents)
    return content
  } catch (error) {
    console.error(`Error reading tutorial content: ${error}`)
    return 'Error loading tutorial content.'
  }
}

export function getTutorialStructure(language: string): Tutorial[] {
  try {
    const decodedLanguage = decodeURIComponent(language)
    const languagePath = path.join(tutorialsDirectory, decodedLanguage)
    return getDirectoryStructure(languagePath)
  } catch (error) {
    console.error(`Error getting tutorial structure: ${error}`)
    return []
  }
}

interface TutorialItem {
  title: string;
  path: string;
  items?: TutorialItem[];
}

function getDirectoryStructure(dirPath: string): TutorialItem[] {
  const items = fs.readdirSync(dirPath, { withFileTypes: true })
  const structure = items.map(item => {
    const itemPath = path.join(dirPath, item.name)
    if (item.isDirectory()) {
      return {
        title: item.name,
        path: path.relative(tutorialsDirectory, itemPath),
        items: getDirectoryStructure(itemPath)
      }
    } else if (item.isFile() && item.name.endsWith('.md')) {
      const fileContents = fs.readFileSync(itemPath, 'utf8')
      const { data } = matter(fileContents)
      return {
        title: data.title || item.name.replace('.md', ''),
        path: path.relative(tutorialsDirectory, itemPath).replace('.md', '')
      }
    }
    return null
  }).filter((item): item is TutorialItem => item !== null)

  // Then, sort the items
  structure.sort((a, b) => {
    // If both items are directories (have 'items' property)
    if (a.items && b.items) {
      // Extract numbers from the beginning of the title
      const aMatch = a.title.match(/^(\d+)/)
      const bMatch = b.title.match(/^(\d+)/)
      
      if (aMatch && bMatch) {
        // If both have numbers, compare them numerically
        return parseInt(aMatch[1]) - parseInt(bMatch[1])
      } else if (aMatch) {
        // If only a has a number, it should come first
        return -1
      } else if (bMatch) {
        // If only b has a number, it should come first
        return 1
      }
    }
    
    // If one is a directory and the other is not, put directories first
    if (a.items && !b.items) return -1
    if (!a.items && b.items) return 1

    // For non-directories or if both are directories without numbers
    return a.title.localeCompare(b.title)
  })

  // Finally, move "大纲" to the front if it exists
  const outlineIndex = structure.findIndex(item => item.title.includes('大纲'))
  if (outlineIndex !== -1) {
    const [outline] = structure.splice(outlineIndex, 1)
    structure.unshift(outline)
  }

  return structure
}

export function getAvailableLanguages() {
  try {
    const languages = fs.readdirSync(tutorialsDirectory)
      .filter(item => {
        const stats = fs.statSync(path.join(tutorialsDirectory, item))
        return stats.isDirectory() && !item.startsWith('.')
      })
    
    // Sort languages based on the order defined in languageOrder
    return languages.sort((a, b) => {
      const indexA = languageOrder.indexOf(a)
      const indexB = languageOrder.indexOf(b)
      if (indexA === -1 && indexB === -1) return a.localeCompare(b)
      if (indexA === -1) return 1
      if (indexB === -1) return -1
      return indexA - indexB
    }).map(lang => encodeURIComponent(lang))
  } catch (error) {
    console.error(`Error getting available languages: ${error}`)
    return []
  }
}