'use client'

import TutorialSidebar from '@/components/TutorialSidebar'

export default function LanguageLayoutClient({ 
  children,
  tutorials,
  language
}: { 
  children: React.ReactNode,
  tutorials: any[],
  language: string
}) {
  return (
    <div className="flex">
      <TutorialSidebar tutorials={tutorials} language={language} />
      <main className="flex-1 p-4 overflow-auto h-[calc(100vh-8rem)]">
        {children}
      </main>
    </div>
  )
}