import type { Doc } from "@/lib/content";
import { SITE } from "@/lib/site";
import { AUTHOR_LD, bookFor } from "@/lib/author";
import { BookCard, AuthorBox } from "./AuthorCard";
import Toc from "./Toc";

export default function DocPage({
  doc, head, side, cards, after, lead, jsonLd, tocFirst = true,
}: {
  doc: Doc;
  head: React.ReactNode;
  side?: React.ReactNode;
  cards?: React.ReactNode;   // 正文下面并排的卡片（这一关会用到的词｜下一关）
  after?: React.ReactNode;   // 纸张里、正文后面（过关条）
  lead?: React.ReactNode;
  jsonLd?: Record<string, unknown>;
  tocFirst?: boolean;
}) {
  const ld = jsonLd ?? {
    "@context": "https://schema.org", "@type": "Article", headline: doc.title, description: doc.summary,
    inLanguage: "zh-CN", dateModified: doc.updated, author: AUTHOR_LD,
    publisher: { "@type": "Organization", name: SITE.name, url: SITE.url },
  };
  return (
    <article>
      <script type="application/ld+json" dangerouslySetInnerHTML={{ __html: JSON.stringify(ld) }} />
      {head}
      <div className="wrap wide l-grid">
        <div className="main-col">
          <div className="sheet">
            {lead}
            <div className="prose" id="prose" dangerouslySetInnerHTML={{ __html: doc.body }} />
            {after}
          </div>
          {cards}
          <AuthorBox />
          <p className="updated">发现过时或写错的地方，欢迎在 <a href="https://github.com/alchaincyf/CoderWithAI/issues" target="_blank" rel="noopener">GitHub</a> 告诉我们。</p>
        </div>
        <aside className="l-side" aria-label="辅助信息">
          {tocFirst && <Toc headings={doc.headings} />}
          {side}
          <BookCard id={bookFor(doc.section, doc.slug)} />
        </aside>
      </div>
    </article>
  );
}
