import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import SideRescue from "@/components/SideRescue";
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
      doc={doc} color="lemon"
      crumbs={[{ href: "/", label: "首页" }, { href: "/recipes", label: "项目配方" }]}
      facts={[
        { k: "难度", v: DIFF[Number(doc.data.difficulty)] ?? "" },
        { k: "用时", v: String(doc.data.time ?? "") },
        { k: "花费", v: String(doc.data.cost ?? "") },
        ...(lv ? [{ k: "建议先过", v: `第${lv.n}关` }] : []),
      ]}
      side={<>
        {lv && <div className="side-card"><h3>开始前</h3><p>这个配方默认你已经过了<Link href={`/start/${lv.slug}`}>第{lv.n}关「{lv.title}」</Link>。没过也能试，卡住了再回去补。</p></div>}
        {others.length > 0 && <div className="side-card"><h3>同难度配方</h3><ul>{others.map((d) => <li key={d.slug}><Link href={hrefOf(d)}>{d.title}</Link></li>)}</ul></div>}
        <SideRescue />
      </>}
    />
  );
}
