import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import ChipLinks from "@/components/ChipLinks";
import SideRescue from "@/components/SideRescue";
import { getDoc, getDocs } from "@/lib/content";
import { docMeta } from "@/lib/meta";
import { LEVELS } from "@/lib/levels";

export const dynamicParams = false;
export function generateStaticParams() { return getDocs("words").map((d) => ({ slug: d.slug })); }
export async function generateMetadata({ params }: { params: Promise<{ slug: string }> }) {
  const d = getDoc("words", (await params).slug);
  return docMeta(d, d ? `${d.title}是什么？人话解释` : undefined);
}

export default async function Page({ params }: { params: Promise<{ slug: string }> }) {
  const doc = getDoc("words", (await params).slug);
  if (!doc) notFound();
  const lv = LEVELS.find((l) => l.n === Number(doc.data.level));
  const ld = {
    "@context": "https://schema.org", "@type": "DefinedTerm", name: doc.title, description: doc.summary,
    inDefinedTermSet: { "@type": "DefinedTermSet", name: "CoderWithAI 人话词典", url: "https://www.coderwithai.top/words" },
  };
  return (
    <DocPage
      doc={doc} color="ink" jsonLd={ld}
      crumbs={[{ href: "/", label: "首页" }, { href: "/words", label: "人话词典" }]}
      aka={doc.data.aka}
      facts={[{ k: "分类", v: String(doc.data.category ?? "") }, ...(lv ? [{ k: "第一次碰到", v: `第${lv.n}关` }] : [])]}
      after={<ChipLinks title="相关的词" section="words" slugs={doc.data.related} />}
      side={<>
        {lv && <div className="side-card"><h3>在路线里</h3><p>你会在<Link href={`/start/${lv.slug}`}>第{lv.n}关「{lv.title}」</Link>第一次碰到它。</p></div>}
        <SideRescue />
      </>}
    />
  );
}
