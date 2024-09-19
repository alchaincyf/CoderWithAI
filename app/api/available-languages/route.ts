import { NextResponse } from 'next/server'
import fs from 'fs'
import path from 'path'

const tutorialsDirectory = path.join(process.cwd(), 'tutorials')

export async function GET() {
  try {
    const languages = fs.readdirSync(tutorialsDirectory)
      .filter(item => {
        const stats = fs.statSync(path.join(tutorialsDirectory, item))
        return stats.isDirectory() && !item.startsWith('.')
      })
    return NextResponse.json(languages)
  } catch (error) {
    console.error(`Error getting available languages: ${error}`)
    return NextResponse.json({ error: 'Error loading available languages' }, { status: 500 })
  }
}