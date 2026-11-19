"use client";
import Link from "next/link";
import { LEVELS } from "@/lib/levels";
import { LEVEL_ART } from "@/lib/level-art";
import { readDone, writeDone, useDone } from "./progress";
import { track } from "./track";
import { burstAt } from "./burst";
import Mk from "./Mk";

const CHECK = <svg viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="2.4" strokeLinecap="round" strokeLinejoin="round" aria-hidden="true"><path d="M3 8.5l3.2 3L13 4.5" /></svg>;

// 路线：第一段向右，第二段折返向左；第3关是蓝色里程碑，第7关盖「终」字印章；过关后虚线变实线。
export default function RouteMap({ times, showOut = false, head = true }: { times: Record<string, string>; showOut?: boolean; head?: boolean }) {
  const done = useDone();
  const toggle = (slug: string, el: HTMLElement) => {
    const cur = readDone();
    const was = cur.includes(slug);
    if (!was) { track("level_done", { level: slug }); burstAt(el, 14); }
    writeDone(was ? cur.filter((s) => s !== slug) : [...cur, slug]);
  };
  const n = LEVELS.filter((l) => done.includes(l.slug)).length;
  const card = (l: (typeof LEVELS)[number]) => {
    const isDone = done.includes(l.slug);
    const t = times[l.slug]?.replace(/（.*?）/g, "");
    return (
      <article key={l.slug} className={`st${l.n === 3 ? " ms" : ""}${isDone ? " done" : ""}`}>
        {l.n === 3 && <span className="flag">从这里有网址</span>}
        {l.n === 7 && <span className="stamp" aria-label="终点">终</span>}
        <Link className="st-a" href={`/start/${l.slug}`}>
          <svg className="art" viewBox="0 0 220 112" role="img" aria-label={`第${l.n}关示意图`} dangerouslySetInnerHTML={{ __html: LEVEL_ART[l.n] }} />
          <div className="st-t">
            <span className="num">{l.n}</span>
            <h3>{l.title}</h3>
            <p>{l.sub}</p>
            {showOut && <p className="st-out"><b>过关时你会有</b>{l.out}</p>}
          </div>
        </Link>
        <div className="st-f">
          <span className="cap">{t}</span>
          <button type="button" className="tick" aria-pressed={isDone} aria-label={`第${l.n}关标记为已过关`} onClick={(e) => toggle(l.slug, e.currentTarget)}>
            <span className="c">{CHECK}</span>过关
          </button>
        </div>
      </article>
    );
  };
  const first = LEVELS.slice(0, 4), second = LEVELS.slice(4);
  return (
    <>
      {head && (
        <div className="card route-top">
          <b>已过 {n} / 8 关</b>
          <div className="meter" role="progressbar" aria-valuemin={0} aria-valuemax={8} aria-valuenow={n}><i style={{ width: `${(n / 8) * 100}%` }} /></div>
          <span className="cap">进度只存在你自己的浏览器里，不用注册。</span>
          <button type="button" className="lnk" onClick={() => writeDone([])}>清零</button>
        </div>
      )}
      <p className="act"><b>第一段</b>做出来，拿到一个网址<span className="cap">第0到3关</span></p>
      <div className={`stg${first.every((l) => done.includes(l.slug)) ? " done-v" : ""}`}>{first.map(card)}</div>
      <div className="turn"><p className="act"><b>第二段</b>让它越来越像样<span className="cap">第4到7关</span></p></div>
      <div className="stg rev last">{second.map(card)}</div>
    </>
  );
}

export function RouteTitle() {
  return <>闯关路线：<Mk k="o">八关</Mk></>;
}
