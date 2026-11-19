import Link from "next/link";
import { getDoc, hrefOf, type Section } from "@/lib/content";

export default function ChipLinks({ title, section, slugs }: { title: string; section: Section; slugs?: string[] }) {
  const docs = (slugs ?? []).map((s) => getDoc(section, s)).filter(Boolean);
  if (!docs.length) return null;
  return (
    <div className="related">
      <h2>{title}</h2>
      <div className="chiplinks">{docs.map((d) => <Link key={d!.slug} href={hrefOf(d!)}>{d!.title.replace(/^第\d关 · /, "")}</Link>)}</div>
    </div>
  );
}
