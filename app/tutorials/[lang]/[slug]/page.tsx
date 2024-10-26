import { getTutorialContent } from "@/lib/tutorials"
import ReactMarkdown from 'react-markdown'

interface PageParams {
  lang: string;
  slug: string;
}

export default async function TutorialPage({ params }: { params: PageParams }) {
  const { lang, slug } = params;
  const content = await getTutorialContent(lang, slug);

  if (typeof content !== 'string') {
    return <div>Error: Unable to load tutorial content</div>;
  }

  return (
    <>
      <article className="prose lg:prose-xl">
        <ReactMarkdown>{content}</ReactMarkdown>
      </article>
    </>
  );
}
