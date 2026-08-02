import fs from 'fs'
import path from 'path'
import matter from 'gray-matter'
import { Tutorial } from '@/lib/tutorials'

const tutorialsDirectory = path.join(process.cwd(), 'tutorials')

function getDirectoryStructure(dirPath: string, basePath: string): Tutorial[] {
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
  }).filter((item): item is Tutorial => item !== null)

  return structure
}

export async function LanguageProvider({ language }: { language: string }) {
  // 校验 language 参数：路径穿越或对应目录不存在时返回空列表，避免 readdirSync 抛 ENOENT 导致 500
  if (language.includes('..') || language.includes('/') || language.includes('\\')) {
    return { tutorials: [], language }
  }
  const languagePath = path.join(tutorialsDirectory, language)
  if (!fs.existsSync(languagePath) || !fs.statSync(languagePath).isDirectory()) {
    return { tutorials: [], language }
  }
  const tutorials = getDirectoryStructure(languagePath, language)
  return { tutorials, language }
}