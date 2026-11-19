"use client";
import Link from "next/link";
import { useDone } from "./progress";
import { LEVELS } from "@/lib/levels";

const CHECK = <svg viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="2.6" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true"><path d="M3 8.5l3.2 3L13 4.5" /></svg>;

// 关卡页头的「站点条」：八个站，当前站高亮，过关的站打勾
export default function ProgressStrip({ here }: { here?: string }) {
  const done = useDone();
  return (
    <ol className="stations" aria-label="八关进度">
      {LEVELS.map((l) => (
        <li key={l.slug} className={[done.includes(l.slug) ? "done" : "", here === l.slug ? "cur" : ""].join(" ").trim()}>
          <Link href={`/start/${l.slug}`} title={`第${l.n}关 ${l.title}`} aria-current={here === l.slug ? "page" : undefined}>
            <span className="c"><b>{l.n}</b>{CHECK}</span>
            <span className="lbl">{l.title}</span>
          </Link>
        </li>
      ))}
    </ol>
  );
}

// 侧栏迷你八点 + 进度条
export function Mini8({ here }: { here?: string }) {
  const done = useDone();
  const n = LEVELS.filter((l) => done.includes(l.slug)).length;
  return (
    <div className="card sc">
      <h4>八关进度 <span>已过 {n} / 8</span></h4>
      <div className="mini8">
        {LEVELS.map((l) => (
          <Link key={l.slug} href={`/start/${l.slug}`} title={`第${l.n}关 ${l.title}`} aria-label={`第${l.n}关 ${l.title}`}
            className={[done.includes(l.slug) ? "done" : "", here === l.slug ? "cur" : ""].join(" ").trim()}>{l.n}</Link>
        ))}
      </div>
      <div className="meter"><i style={{ width: `${(n / 8) * 100}%` }} /></div>
    </div>
  );
}
