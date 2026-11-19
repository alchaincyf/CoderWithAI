"use client";
import Link from "next/link";
import { readDone, writeDone, useDone } from "./progress";
import { LEVELS } from "@/lib/levels";
import { track } from "./track";

export default function DoneBar({ slug }: { slug: string }) {
  const done = useDone();
  const isDone = done.includes(slug);
  const idx = LEVELS.findIndex((l) => l.slug === slug);
  const next = LEVELS[idx + 1];
  const toggle = () => {
    const cur = readDone();
    if (!isDone) track("level_done", { level: slug });
    writeDone(isDone ? cur.filter((s) => s !== slug) : [...cur, slug]);
  };
  return (
    <div className={`done-bar${isDone ? " is-done" : ""}`}>
      <p>{isDone ? `第${idx}关，过了。` : `做到「过关标准」了吗？`}</p>
      <div className="row" style={{ gap: 14 }}>
        <button className={`btn sm ${isDone ? "" : "ink"}`} type="button" onClick={toggle}>{isDone ? "撤销" : "我过关了"}</button>
        {next && <Link className="btn sm orange" href={`/start/${next.slug}`}>下一关：{next.title}</Link>}
      </div>
    </div>
  );
}
