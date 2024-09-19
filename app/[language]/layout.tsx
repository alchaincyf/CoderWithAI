'use client'

import { useState, useEffect } from 'react'
import TutorialSidebar from '@/components/TutorialSidebar'

async function fetchLanguageData(language: string) {
  const res = await fetch(`/api/language-layout?language=${encodeURIComponent(language)}`)
  if (!res.ok) throw new Error('Failed to fetch language data')
  return res.json()
}

export default function LanguageLayout({ 
  children,
  params 
}: { 
  children: React.ReactNode,
  params: { language: string }
}) {
  const [data, setData] = useState<{ tutorials: any[], language: string } | null>(null)

  useEffect(() => {
    fetchLanguageData(params.language).then(setData)
  }, [params.language])

  if (!data) return null

  return (
    <div className="flex">
      <TutorialSidebar tutorials={data.tutorials} language={data.language} />
      <main className="flex-1 p-4 overflow-auto h-[calc(100vh-8rem)]">
        {children}
      </main>
    </div>
  )
}