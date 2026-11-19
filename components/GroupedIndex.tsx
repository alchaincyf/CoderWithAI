import Link from "next/link";
import { hrefOf, type Doc } from "@/lib/content";

export default function GroupedIndex({ docs, order, tagOf }: { docs: Doc[]; order: string[]; tagOf?: (d: Doc) => string | undefined }) {
  const groups = new Map<string, Doc[]>();
  for (const d of docs) {
    const c = String(d.data.category ?? "其他");
    if (!groups.has(c)) groups.set(c, []);
    groups.get(c)!.push(d);
  }
  const cats = [...order.filter((c) => groups.has(c)), ...[...groups.keys()].filter((c) => !order.includes(c))];
  return (<>
    <nav className="filters" aria-label="按分类跳转">
      {cats.map((c) => <a key={c} href={`#cat-${c}`}>{c} <span className="note">{groups.get(c)!.length}</span></a>)}
    </nav>
    {cats.map((c) => (
      <section key={c} className="cat-block" id={`cat-${c}`}>
        <h2>{c}<small>{groups.get(c)!.length}条</small></h2>
        <ul className="index-list">
          {groups.get(c)!.map((d) => (
            <li key={d.slug}><Link href={hrefOf(d)}>
              <span className="t">{d.title}</span>{tagOf?.(d) && <span className="k">{tagOf(d)}</span>}
              <span className="s">{d.summary}</span>
            </Link></li>
          ))}
        </ul>
      </section>
    ))}
  </>);
}
