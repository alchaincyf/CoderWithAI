"use client";
import Link from "next/link";
import { useDone } from "./progress";
import { LEVELS } from "@/lib/levels";

export default function RouteList({ times }: { times: Record<string, string> }) {
  const done = useDone();
  return (
    <ol className="route">
      {LEVELS.map((l) => (
        <li key={l.slug} className={done.includes(l.slug) ? "done" : ""}>
          <span className="n">{l.n}</span>
          <div>
            <h2><Link href={`/start/${l.slug}`}>{l.title}</Link></h2>
            <p>{l.sub}{times[l.slug] ? `。大约${times[l.slug]}。` : "。"}</p>
          </div>
          <div className="out"><b>过关时你会有</b>{l.out}</div>
        </li>
      ))}
    </ol>
  );
}
