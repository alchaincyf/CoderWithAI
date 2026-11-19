"use client";
import Link from "next/link";
import { readDone, writeDone, useDone } from "./progress";
import { LEVELS } from "@/lib/levels";
import { track } from "./track";
import { burstAt } from "./burst";

export default function DoneBar({ slug }: { slug: string }) {
  const done = useDone();
  const isDone = done.includes(slug);
  const idx = LEVELS.findIndex((l) => l.slug === slug);
  const next = LEVELS[idx + 1];
  const toggle = (el: HTMLElement) => {
    const cur = readDone();
    if (!isDone) { track("level_done", { level: slug }); burstAt(el, 22); }
    writeDone(isDone ? cur.filter((s) => s !== slug) : [...cur, slug]);
  };
  return (
    <div className="done-card">
      <p><b>{isDone ? `第${idx}关，过了。` : "做到「过关标准」了吗？"}</b>{isDone ? "进度记在你的浏览器里，随时可以撤销。" : "点一下，这一关就记在你的浏览器里。"}</p>
      <div className="done-acts">
        <button id="pass" className="btn" type="button" aria-pressed={isDone} onClick={(e) => toggle(e.currentTarget)}>{isDone ? "撤销过关" : `标记第${idx}关过关`}</button>
        {next && <Link className="btn pri" href={`/start/${next.slug}`}>下一关：{next.title}</Link>}
      </div>
    </div>
  );
}
