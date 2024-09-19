import { getTutorialStructure } from "@/lib/tutorials"
import Link from 'next/link'

export default function LanguagePage({ params }: { params: { language: string } }) {
  return (
    <div>
      <h1 className="text-3xl font-bold mb-4">{decodeURIComponent(params.language)} Tutorials</h1>
      <p>Please select a tutorial from the sidebar.</p>
    </div>
  )
}