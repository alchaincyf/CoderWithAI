import { getTutorialStructure } from '@/lib/tutorials'
import { Suspense } from 'react'
import { notFound } from 'next/navigation'
import ClientSideTutorialTree from './ClientSideTutorialTree'
import { ErrorBoundary } from '../components/ErrorBoundary'

export default async function Page({ params }: { params: { language: string } }) {
  const tutorials = await getTutorialStructure(params.language);
  // 不存在的语言目录返回 404，而不是让 readdirSync 抛异常变成 500
  if (tutorials.length === 0) {
    notFound();
  }
  const language = params.language;

  return (
    <div className="p-4">
      <h1 className="text-3xl font-bold mb-4">{decodeURIComponent(language)} Tutorials</h1>
      <div className="bg-white shadow-md rounded p-6">
        <ErrorBoundary>
          <Suspense fallback={<div>Loading...</div>}>
            <ClientSideTutorialTree tutorials={tutorials} language={language} />
          </Suspense>
        </ErrorBoundary>
      </div>
    </div>
  );
}
