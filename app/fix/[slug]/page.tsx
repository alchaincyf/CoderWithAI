import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import SideRescue from "@/components/SideRescue";
import { FixHead } from "@/components/Heads";
import { getDoc, getDocs, hrefOf } from "@/lib/content";
import { docMeta } from "@/lib/meta";
import { AUTHOR_LD } from "@/lib/author";
import { storiesFor } from "@/lib/stories";
import StoryQuote from "@/components/StoryQuote";

export const dynamicParams = false;
export function generateStaticParams() { return getDocs("fix").map((d) => ({ slug: d.slug })); }
export async function generateMetadata({ params }: { params: Promise<{ slug: string }> }) {
  return docMeta(getDoc("fix", (await params).slug));
}

const PLAT: Record<string, string> = { mac: "Mac", windows: "Windows", all: "所有系统", linux: "Linux" };

export default async function Page({ params }: { params: Promise<{ slug: string }> }) {
  const doc = getDoc("fix", (await params).slug);
  if (!doc) notFound();
  const same = getDocs("fix").filter((d) => d.data.category === doc.data.category && d.slug !== doc.slug).slice(0, 6);
  const plats = (doc.data.platform ?? []).map((p: string) => PLAT[p] ?? p).join("、");
  const ld = {
    "@context": "https://schema.org", "@type": "TechArticle", headline: doc.title, description: doc.summary,
    inLanguage: "zh-CN", dateModified: doc.updated, proficiencyLevel: "Beginner", author: AUTHOR_LD,
  };
  const kw = ((doc.data.match ?? []) as string[]).map(String);
  return (
    <DocPage
      doc={doc} jsonLd={ld}
      head={<FixHead doc={doc} plats={plats ? plats.split("、") : []} />}
      side={<>
        {kw.length > 0 && (
          <div className="card sc"><h4>对照关键词</h4><p className="sub">你的红字里出现任何一个，就是这一条。</p>
            <div className="kw">{kw.map((k) => <span key={k} className="chip mono">{k}</span>)}</div>
          </div>
        )}
        {same.length > 0 && (
          <div className="card sc"><h4>同类报错 <span>{String(doc.data.category ?? "")}</span></h4>
            <ul className="rel-l">{same.map((d) => <li key={d.slug}><Link href={hrefOf(d)}>{d.title}</Link></li>)}</ul>
          </div>
        )}
        {storiesFor(`fix-${doc.slug}`).slice(0, 1).map((st) => <StoryQuote key={st.quote} story={st} compact />)}
        <SideRescue />
      </>}
    />
  );
}
