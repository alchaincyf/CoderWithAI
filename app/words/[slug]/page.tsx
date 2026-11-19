import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import ChipLinks from "@/components/ChipLinks";
import SideRescue from "@/components/SideRescue";
import { PlainHead } from "@/components/Heads";
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
      doc={doc} jsonLd={ld}
      head={<PlainHead
        crumbs={[{ href: "/", label: "首页" }, { href: "/words", label: "人话词典" }, { label: doc.title }]}
        kicker={`人话词典${doc.data.category ? "：" + doc.data.category : ""}`} title={doc.title} summary={doc.summary} aka={doc.data.aka} updated={doc.updated}
        meta={lv ? [{ k: "第一次碰到", v: `第${lv.n}关` }] : []} />}
      cards={<ChipLinks title="相关的词" section="words" slugs={doc.data.related} />}
      side={<>
        {lv && <div className="card sc"><h4>在路线里</h4><p className="sub">你会在<Link href={`/start/${lv.slug}`}>第{lv.n}关「{lv.title}」</Link>第一次碰到它。</p></div>}
        <SideRescue />
      </>}
    />
  );
}
