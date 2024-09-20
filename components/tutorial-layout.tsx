'use client'

import React from 'react'

export default function TutorialLayout({ children }: { children: React.ReactNode }) {
  return (
    <div className="flex min-h-screen">
      <main className="flex-1 p-4">
        {children}
      </main>
    </div>
  )
}