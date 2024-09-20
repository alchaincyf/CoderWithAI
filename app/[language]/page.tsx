import { getTutorialStructure } from '@/lib/tutorials'
// 删除未使用的 Link 导入
import { Suspense } from 'react'
import ClientSideTutorialTree from './ClientSideTutorialTree'
import GoogleAds from '@/components/GoogleAds';

// 删除未使用的 TutorialItem 接口

export default async function Page({ params }: { params: { language: string } }) {
  const tutorials = await getTutorialStructure(params.language);
  const language = params.language;

  return (
    <div className="p-4">
      <h1 className="text-3xl font-bold mb-4">{decodeURIComponent(language)} Tutorials</h1>
      <GoogleAds />
      <div className="bg-white shadow-md rounded p-6">
        <Suspense fallback={<div>Loading...</div>}>
          <ClientSideTutorialTree tutorials={tutorials} language={language} />
        </Suspense>
      </div>
      <GoogleAds />
    </div>
  );
}