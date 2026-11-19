"use client";
import Link from "next/link";
import { useDone } from "./progress";
import { LEVELS } from "@/lib/levels";

export default function ProgressStrip({ here }: { here?: string }) {
  const done = useDone();
  return (
    <nav className="progress-strip" aria-label="八关进度">
      {LEVELS.map((l) => (
        <Link key={l.slug} href={`/start/${l.slug}`} title={`第${l.n}关 ${l.title}`}
          className={[done.includes(l.slug) ? "done" : "", here === l.slug ? "here" : ""].join(" ")}>{l.n}</Link>
      ))}
    </nav>
  );
}
