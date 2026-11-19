import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import SideRescue from "@/components/SideRescue";
import { PlainHead } from "@/components/Heads";
import { getDoc, getDocs, hrefOf } from "@/lib/content";
import { docMeta } from "@/lib/meta";
import { LEVELS } from "@/lib/levels";

export const dynamicParams = false;
export function generateStaticParams() { return getDocs("recipes").map((d) => ({ slug: d.slug })); }
export async function generateMetadata({ params }: { params: Promise<{ slug: string }> }) {
  return docMeta(getDoc("recipes", (await params).slug));
}
const DIFF = ["", "入门", "进阶", "挑战"];

export default async function Page({ params }: { params: Promise<{ slug: string }> }) {
  const doc = getDoc("recipes", (await params).slug);
  if (!doc) notFound();
  const lv = LEVELS.find((l) => l.n === Number(doc.data.level));
  const others = getDocs("recipes").filter((d) => d.slug !== doc.slug && d.data.difficulty === doc.data.difficulty).slice(0, 5);
  return (
    <DocPage
      doc={doc}
      head={<PlainHead
        crumbs={[{ href: "/", label: "首页" }, { href: "/projects", label: "实战项目" }, { label: doc.title }]}
        kicker="实战项目" title={doc.title} summary={doc.summary} updated={doc.updated}
        meta={[
          { k: "难度", v: DIFF[Number(doc.data.difficulty)] ?? "" },
          { k: "用时", v: String(doc.data.time ?? "") },
          { k: "花费", v: String(doc.data.cost ?? "") },
          ...(lv ? [{ k: "建议先过", v: `第${lv.n}关` }] : []),
        ]} />}
      side={<>
        {lv && <div className="card sc"><h4>开始前</h4><p className="sub">这个项目默认你已经过了<Link href={`/start/${lv.slug}`}>第{lv.n}关「{lv.title}」</Link>。没过也能试，卡住了再回去补。</p></div>}
        {others.length > 0 && <div className="card sc"><h4>同难度项目</h4><ul className="stuck">{others.map((d) => <li key={d.slug}><Link href={hrefOf(d)}>{d.title}</Link></li>)}</ul></div>}
        <SideRescue />
      </>}
    />
  );
}
