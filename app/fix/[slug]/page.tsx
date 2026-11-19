import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import SideRescue from "@/components/SideRescue";
import { getDoc, getDocs, hrefOf } from "@/lib/content";
import { docMeta } from "@/lib/meta";
import { AUTHOR_LD } from "@/lib/author";

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
  return (
    <DocPage
      doc={doc} color="orange" jsonLd={ld}
      crumbs={[{ href: "/", label: "首页" }, { href: "/fix", label: "报错急救" }]}
      facts={[{ k: "分类", v: String(doc.data.category ?? "") }, ...(plats ? [{ k: "系统", v: plats }] : [])]}
      side={<>
        {same.length > 0 && (
          <div className="side-card"><h3>同类报错</h3>
            <ul>{same.map((d) => <li key={d.slug}><Link href={hrefOf(d)}>{d.title}</Link></li>)}</ul>
          </div>
        )}
        <SideRescue />
      </>}
    />
  );
}
