"use client";
import Link from "next/link";
import { useEffect, useMemo, useRef, useState } from "react";
import { useRouter } from "next/navigation";
import { search, matchError, SECTION_LABEL, type IndexItem } from "@/lib/match";
import { useIndex } from "./useIndex";

const HOT = ["claude code 怎么装", "部署", "环境变量", "command not found", "个人主页", "API Key"];

export default function SearchPanel({ onClose }: { onClose: () => void }) {
  const items = useIndex();
  const [q, setQ] = useState("");
  const [active, setActive] = useState(0);
  const ref = useRef<HTMLTextAreaElement>(null);
  const router = useRouter();

  useEffect(() => {
    ref.current?.focus();
    const onKey = (e: KeyboardEvent) => { if (e.key === "Escape") onClose(); };
    window.addEventListener("keydown", onKey);
    document.body.style.overflow = "hidden";
    return () => { window.removeEventListener("keydown", onKey); document.body.style.overflow = ""; };
  }, [onClose]);

  const looksLikeError = q.length > 40 || /error|err!|fatal|failed|exception|not found|denied|报错|错误/i.test(q);
  const results: IndexItem[] = useMemo(() => {
    if (!items || !q.trim()) return [];
    const errs = looksLikeError ? matchError(items, q) : [];
    const rest = search(items, q, 12).filter((r) => !errs.includes(r));
    return [...errs, ...rest].slice(0, 12);
  }, [items, q, looksLikeError]);
  useEffect(() => setActive(0), [q]);

  const go = (h: string) => { onClose(); router.push(h); };

  return (
    <div className="search-layer" onMouseDown={(e) => { if (e.target === e.currentTarget) onClose(); }}>
      <div className="search-panel" role="dialog" aria-modal="true" aria-label="站内搜索">
        <div className="field">
          <label htmlFor="sq" className="sr">搜索</label>
          <textarea
            id="sq" ref={ref} rows={1} value={q} placeholder="想做什么，或者卡在哪了？报错可以整段贴进来"
            onChange={(e) => setQ(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "ArrowDown") { e.preventDefault(); setActive((a) => Math.min(a + 1, results.length - 1)); }
              if (e.key === "ArrowUp") { e.preventDefault(); setActive((a) => Math.max(a - 1, 0)); }
              if (e.key === "Enter" && !e.shiftKey && results[active]) { e.preventDefault(); go(results[active].h); }
            }}
          />
          <button className="btn ghost esc" type="button" onClick={onClose}>关闭</button>
        </div>
        <div className="search-results">
          {!items && <p className="search-hint">正在载入索引……</p>}
          {items && !q.trim() && (
            <>
              <p className="search-group">大家常搜</p>
              <div className="search-hint chiplinks">
                {HOT.map((h) => <button key={h} type="button" className="chip" onClick={() => setQ(h)}>{h}</button>)}
              </div>
            </>
          )}
          {items && q.trim() && results.length === 0 && (
            <p className="search-hint">
              没找到现成的条目。{looksLikeError ? <>报错可以去 <Link href="/fix" onClick={onClose}>报错急救</Link> 拿一段通用的求助提示词，原样发给AI。</> : "换个说法试试，比如用更短的词。"}
            </p>
          )}
          {results.map((r, i) => (
            <Link key={r.h} href={r.h} className="search-item" data-active={i === active} onClick={onClose} onMouseEnter={() => setActive(i)}>
              <span className="pill plain">{SECTION_LABEL[r.s]}</span>
              <span><span className="t">{r.t}</span><span className="s">{r.d}</span></span>
            </Link>
          ))}
        </div>
      </div>
    </div>
  );
}
