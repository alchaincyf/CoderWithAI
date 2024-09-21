import SEO from '../../../components/SEO';
import { getTutorialContent } from '@/lib/tutorials';

interface PageParams {
  lang: string;
  slug: string;
}

interface Tutorial {
  title: string;
  summary: string;
  tags: string[];
}

export default function TutorialPage({ params }: { params: PageParams }) {
  const { lang, slug } = params;
  const tutorial = getTutorialContent(lang, slug) as Tutorial;

  return (
    <>
      <SEO 
        title={`${tutorial.title} - CoderWithAI`}
        description={tutorial.description}
        keywords={`${lang},编程教程,${tutorial.tags.join(',')},${tutorial.keywords.join(',')}`}
        canonicalUrl={`https://www.coderwith.ai/tutorials/${lang}/${slug}`}
        datePublished={tutorial.date}
        category={tutorial.category}
      />
      {/* 教程内容 */}
    </>
  );
}