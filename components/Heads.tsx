import Link from "next/link";
import Mk from "./Mk";
import ProgressStrip from "./ProgressStrip";
import { Byline } from "./AuthorCard";
import { termHtml } from "@/lib/term";
import type { Doc } from "@/lib/content";

export interface Crumb { href?: string; label: string }

export function Crumbs({ items }: { items: Crumb[] }) {
  return (
    <nav className="crumb" aria-label="位置">
      {items.map((c, i) => (
        <span key={c.label + i} className="crumb-i">
          {i > 0 && <span aria-hidden="true">/</span>}
          {c.href ? <Link href={c.href}>{c.label}</Link> : <span>{c.label}</span>}
        </span>
      ))}
    </nav>
  );
}

// 通用页头：词典、实战项目
export function PlainHead({ crumbs, kicker, title, summary, aka, updated, meta }: { crumbs: Crumb[]; kicker: string; title: string; summary?: string; aka?: string[]; updated: string; meta?: { k: string; v: React.ReactNode }[] }) {
  const facts = (meta ?? []).filter((m) => m.v);
  return (
    <header className="p-head">
      <div className="wrap wide">
        <Crumbs items={crumbs} />
        <span className="pill">{kicker}</span>
        <h1>{title}</h1>
        {aka && aka.length > 0 && <p className="aka">也叫：{aka.join(" · ")}</p>}
        {summary && <p className="sum">{summary}</p>}
        <Byline updated={updated} />
        {facts.length > 0 && <div className="card l-meta flex">{facts.map((m) => <div key={m.k}><b>{m.k}</b>{m.v}</div>)}</div>}
      </div>
    </header>
  );
}

// 关卡页头：大号关数 + 三格要点 + 站点条
export function PathHead({ doc, n }: { doc: Doc; n: number }) {
  const title = doc.title.replace(/^第\d+关\s*[·:：]\s*/, "");
  return (
    <header className="l-head">
      <div className="wrap wide">
        <Crumbs items={[{ href: "/", label: "首页" }, { href: "/start", label: "闯关路线" }, { label: `第${n}关` }]} />
        <div className="l-title">
          <div className="l-no" aria-hidden="true">{n}<small>关</small></div>
          <div>
            <h1>{title}</h1>
            <p className="sum">{doc.summary}</p>
            <Byline updated={doc.updated} />
          </div>
        </div>
        <div className="card l-meta">
          <div><b>要花多久</b>{String(doc.data.time ?? "")}</div>
          <div><b>要花多少钱</b>{String(doc.data.cost ?? "")}</div>
          <div><b>过关标准</b>{String(doc.data.goal ?? "")}</div>
        </div>
        <ProgressStrip here={doc.slug} />
      </div>
    </header>
  );
}

// 报错页头：左边标题，右边是「你的终端里是不是这样」，朱笔圈出关键一句
function pickTerms(doc: Doc) {
  return ((doc.data.match ?? []) as string[]).map(String).filter((m) => m.length >= 3).sort((a, b) => b.length - a.length);
}
export function circleOf(doc: Doc): string | undefined {
  const text = doc.errorText;
  if (!text) return undefined;
  const terms = pickTerms(doc);
  const lines = text.split("\n");
  for (let i = lines.length - 1; i >= 0; i--) {
    const low = lines[i].toLowerCase();
    for (const t of terms) {
      const at = low.indexOf(t.toLowerCase());
      if (at >= 0) return lines[i].slice(at, at + t.length);
    }
  }
  return undefined;
}

export function FixHead({ doc, plats }: { doc: Doc; plats: string[] }) {
  const terms = pickTerms(doc);
  // 只圈短词：长词会折行，圈会被压成一条竖线
  const t = terms.find((m) => doc.title.includes(m) && m.length <= 10);
  const at = t ? doc.title.indexOf(t) : -1;
  const circle = circleOf(doc);
  return (
    <header className="f-head">
      <div className="wrap wide">
        <Crumbs items={[{ href: "/", label: "首页" }, { href: "/fix", label: "报错急救" }, ...(doc.data.category ? [{ href: `/fix#cat-${doc.data.category}`, label: String(doc.data.category) }] : []), { label: doc.slug }]} />
        <div className="f-top">
          <div>
            <span className="pill">报错急救：{String(doc.data.category ?? "")}</span>
            <h1>{at >= 0 ? <>{doc.title.slice(0, at)}<Mk k="o">{t!}</Mk>{doc.title.slice(at + t!.length)}</> : doc.title}</h1>
            <p className="sum">{doc.summary}</p>
            <div className="f-tags">
              {plats.map((p) => <span key={p} className="pill plain">{p}</span>)}
              <span className="pill plain">更新于 {doc.updated}</span>
            </div>
            <Byline updated={doc.updated} />
          </div>
          {doc.errorText && (
            <div className="f-term" dangerouslySetInnerHTML={{ __html: termHtml({ kind: "err", text: doc.errorText, title: "你的终端里是不是这样", tag: "报错原文", circle }) }} />
          )}
        </div>
      </div>
    </header>
  );
}
