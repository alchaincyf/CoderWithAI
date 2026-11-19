"use client";
import { useEffect, useState } from "react";

// 本页目录：滚到哪一节，哪一节高亮
export default function Toc({ headings }: { headings: { id: string; text: string }[] }) {
  const [on, setOn] = useState(headings[0]?.id);
  useEffect(() => {
    const els = headings.map((h) => document.getElementById(h.id)).filter(Boolean) as HTMLElement[];
    if (!els.length || !("IntersectionObserver" in window)) return;
    const vis = new Map<string, boolean>();
    const io = new IntersectionObserver((es) => {
      es.forEach((e) => vis.set(e.target.id, e.isIntersecting));
      const first = els.find((el) => vis.get(el.id));
      if (first) setOn(first.id);
    }, { rootMargin: "-80px 0px -65% 0px" });
    els.forEach((el) => io.observe(el));
    return () => io.disconnect();
  }, [headings]);
  if (headings.length < 3) return null;
  return (
    <div className="card sc">
      <h4>本页目录</h4>
      <ul className="toc">
        {headings.map((h) => <li key={h.id}><a href={`#${h.id}`} className={on === h.id ? "on" : undefined} aria-current={on === h.id ? "location" : undefined}>{h.text}</a></li>)}
      </ul>
    </div>
  );
}
