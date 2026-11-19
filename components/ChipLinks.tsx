import Link from "next/link";
import { getDoc, hrefOf, type Section } from "@/lib/content";

// chips：一行小标签；cards：带一句话解释的卡片（这一关会用到的词）；stuck：侧栏「最常卡住」列表
export default function ChipLinks({ title, section, slugs, variant = "chips", note }: { title: string; section: Section; slugs?: string[]; variant?: "chips" | "cards" | "stuck"; note?: string }) {
  const docs = (slugs ?? []).map((s) => getDoc(section, s)).filter(Boolean) as NonNullable<ReturnType<typeof getDoc>>[];
  if (!docs.length) return null;
  const name = (t: string) => t.replace(/^第\d关 · /, "");
  if (variant === "cards") return (
    <div className="card sc">
      <h4>{title} <span>{docs.length}个</span></h4>
      <ul className="words">
        {docs.map((d) => <li key={d.slug}><Link href={hrefOf(d)}><b>{name(d.title)}</b><span>{d.summary}</span></Link></li>)}
      </ul>
    </div>
  );
  if (variant === "stuck") return (
    <div className="card sc">
      <h4>{title} <span>{note}</span></h4>
      <ul className="stuck">
        {docs.map((d) => <li key={d.slug}><Link href={hrefOf(d)}>{name(d.title)}</Link></li>)}
      </ul>
    </div>
  );
  return (
    <div className="related">
      <h2>{title}</h2>
      <div className="chiplinks">{docs.map((d) => <Link key={d.slug} className="chip" href={hrefOf(d)}>{name(d.title)}</Link>)}</div>
    </div>
  );
}
