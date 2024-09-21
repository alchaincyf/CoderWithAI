import { languageOrder } from '@/config/languageOrder'
import fs from 'fs'
import path from 'path'
import matter from 'gray-matter'

export interface Tutorial {
  title: string;
  path: string;
  items?: Tutorial[];
  sortOrder: number;
  isOutline: boolean;
}

export function sortLanguages(languages: string[]): string[] {
  return languages.sort((a, b) => {
    const indexA = languageOrder.indexOf(a)
    const indexB = languageOrder.indexOf(b)
    if (indexA === -1 && indexB === -1) return a.localeCompare(b)
    if (indexA === -1) return 1
    if (indexB === -1) return -1
    return indexA - indexB
  })
}

const tutorialsDirectory = path.join(process.cwd(), 'tutorials')

export async function getTutorialContent(language: string, tutorialPath: string): Promise<string> {
  try {
    const basePath = path.join(tutorialsDirectory, language, decodeURIComponent(tutorialPath))
    let fullPath = `${basePath}.md`
    
    console.log('Trying path with .md:', fullPath)
    if (!fs.existsSync(fullPath)) {
      console.log('File not found with .md, trying without extension')
      fullPath = basePath
    }

    console.log('Final path being attempted:', fullPath)
    if (!fs.existsSync(fullPath)) {
      console.error('File not found:', fullPath)
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

export async function getTutorialStructure(language: string): Promise<Tutorial[]> {
  const languagePath = path.join(tutorialsDirectory, language)
  return getDirectoryStructure(languagePath, language)
}

function getDirectoryStructure(dirPath: string, basePath: string): Tutorial[] {
  const items = fs.readdirSync(dirPath, { withFileTypes: true })
  const structure = items.map(item => {
    const itemPath = path.join(dirPath, item.name)
    const relativePath = path.relative(path.join(tutorialsDirectory, basePath), itemPath)
    const sortOrderMatch = item.name.match(/^(\d+)/)
    const sortOrder = sortOrderMatch ? parseInt(sortOrderMatch[1], 10) : Infinity

    if (item.isDirectory()) {
      const children = getDirectoryStructure(itemPath, basePath)
      return {
        title: item.name,
        path: relativePath,
        items: children,
        sortOrder: sortOrder,
        isOutline: false
      }
    } else if (item.isFile() && item.name.endsWith('.md')) {
      try {
        const fileContents = fs.readFileSync(itemPath, 'utf8')
        const { data } = matter(fileContents)
        const isOutline = !relativePath.includes(path.sep) || item.name.toLowerCase().includes('大纲')
        return {
          title: data.title || item.name.replace('.md', ''),
          path: relativePath.replace('.md', ''),
          isOutline: isOutline,
          sortOrder: isOutline ? -1 : sortOrder
        }
      } catch (error) {
        console.error(`Error parsing file ${itemPath}:`, error)
        return {
          title: item.name.replace('.md', ''),
          path: relativePath.replace('.md', ''),
          isOutline: false,
          sortOrder: sortOrder
        }
      }
    }
    return null
  }).filter((item): item is Tutorial => item !== null)

  // 修改排序逻辑
  structure.sort((a, b) => {
    if (a.isOutline && !b.isOutline) return -1
    if (!a.isOutline && b.isOutline) return 1
    return a.sortOrder - b.sortOrder
  })

  return structure
}

// 将 getAvailableLanguages 函数移到文件底部
export async function getAvailableLanguages(): Promise<string[]> {
  try {
    const languages = fs.readdirSync(tutorialsDirectory)
      .filter(item => {
        const stats = fs.statSync(path.join(tutorialsDirectory, item))
        return stats.isDirectory() && !item.startsWith('.')
      })
    return sortLanguages(languages)
  } catch (error) {
    console.error(`Error getting available languages: ${error}`)
    return []
  }
}

export async function getTutorialMetadata(language: string, tutorialPath: string) {
  const fullPath = path.join(process.cwd(), 'tutorials', language, `${tutorialPath}.md`)
  const fileContents = await fs.promises.readFile(fullPath, 'utf8')
  const { data } = matter(fileContents)

  // 从路径中提取类别（如果存在）
  const pathParts = tutorialPath.split('/')
  const category = pathParts.length > 1 ? pathParts[pathParts.length - 2] : ''

  return {
    title: data.title || 'CoderWithAI Tutorial',
    description: data.description || `Learn ${language} programming with CoderWithAI`,
    keywords: data.keywords || ['programming', 'tutorial', 'coding', language],
    image: data.image || null,
    category: category,
    // 添加其他你想要的元数据字段
  }
}

// 添加这个函数的导出
export async function getTutorials(language: string): Promise<Tutorial[]> {
  return getTutorialStructure(language);
}