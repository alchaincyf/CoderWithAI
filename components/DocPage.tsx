import Link from "next/link";
import type { Doc } from "@/lib/content";
import { SITE } from "@/lib/site";
import { AUTHOR_LD, bookFor } from "@/lib/author";
import { Byline, BookCard, AuthorBox } from "./AuthorCard";

export interface Crumb { href: string; label: string }

export default function DocPage({
  doc, color, crumbs, facts, aka, side, after, before, lead, jsonLd,
}: {
  doc: Doc;
  color: "blue" | "lemon" | "orange" | "ink" | "white";
  crumbs: Crumb[];
  facts?: { k: string; v: string }[];
  aka?: string[];
  side?: React.ReactNode;
  after?: React.ReactNode;
  before?: React.ReactNode;
  lead?: React.ReactNode;
  jsonLd?: Record<string, unknown>;
}) {
  const ld = jsonLd ?? {
    "@context": "https://schema.org", "@type": "Article", headline: doc.title, description: doc.summary,
    inLanguage: "zh-CN", dateModified: doc.updated, author: AUTHOR_LD,
    publisher: { "@type": "Organization", name: SITE.name, url: SITE.url },
  };
  return (
    <article>
      <script type="application/ld+json" dangerouslySetInnerHTML={{ __html: JSON.stringify(ld) }} />
      <header className={`doc-head ${color}`}>
        <div className="wrap">
          <nav className="crumbs" aria-label="位置">
            {crumbs.map((c, i) => <span key={c.href}>{i > 0 && "／ "}<Link href={c.href}>{c.label}</Link></span>)}
          </nav>
          <h1>{doc.title}</h1>
          {aka && aka.length > 0 && <p className="aka">也叫：{aka.join(" · ")}</p>}
          {doc.summary && <p className="dek">{doc.summary}</p>}
          <Byline updated={doc.updated} />
          {facts && facts.length > 0 && (
            <dl className="facts">{facts.map((f) => <div key={f.k}><dt>{f.k}</dt><dd>{f.v}</dd></div>)}</dl>
          )}
          {before}
        </div>
      </header>
      <div className="doc-body">
        <div className="wrap">
          {doc.headings.length > 2 ? (
            <nav className="toc" aria-label="本页目录">
              <b>本页目录</b>
              <ol>{doc.headings.map((h) => <li key={h.id}><a href={`#${h.id}`}>{h.text}</a></li>)}</ol>
            </nav>
          ) : <div className="toc" />}
          <div>
            {lead}
            <div className="prose" dangerouslySetInnerHTML={{ __html: doc.body }} />
            {after}
            <AuthorBox />
            <p className="updated">发现过时或写错的地方，欢迎在 <a href="https://github.com/alchaincyf/CoderWithAI/issues" target="_blank" rel="noopener">GitHub</a> 告诉我们。</p>
          </div>
          <aside className="side">{side}<BookCard id={bookFor(doc.section, doc.slug)} /></aside>
        </div>
      </div>
    </article>
  );
}
