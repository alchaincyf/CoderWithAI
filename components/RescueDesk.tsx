"use client";
import Link from "next/link";
import { useEffect, useState } from "react";
import { matchError, type IndexItem } from "@/lib/match";
import { useIndex } from "./useIndex";
import { copyText } from "./copy";
import { track } from "./track";

const PRESETS = [
  { label: "npm ERR! code EACCES", text: "npm ERR! code EACCES\nnpm ERR! syscall mkdir\nnpm ERR! Error: EACCES: permission denied, mkdir '/usr/local/lib/node_modules'" },
  { label: "zsh: command not found: npm", text: "zsh: command not found: npm" },
  { label: "EADDRINUSE :::3000", text: "Error: listen EADDRINUSE: address already in use :::3000" },
  { label: "blocked by CORS policy", text: "Access to fetch at 'https://api.example.com/data' from origin 'http://localhost:3000' has been blocked by CORS policy" },
];

function genericPrompt(raw: string) {
  return `我在用AI做一个小项目，运行时报错了。我不会写代码，请你：
1. 先用大白话告诉我这个报错是什么意思、最可能的原因；
2. 再一步一步帮我修好，需要运行的命令请你直接执行，执行前告诉我它是干什么的；
3. 修完后验证一遍，确认真的好了再告诉我。

我当时在做的事：（比如：运行 npm run dev / 部署到Vercel / 安装Claude Code）
报错原文：
${raw || "（把完整的红字原样贴在这里）"}`;
}

export default function RescueDesk({ initial = PRESETS[0].text, compact = false }: { initial?: string; compact?: boolean }) {
  const items = useIndex();
  const [text, setText] = useState(initial);
  const [hits, setHits] = useState<IndexItem[] | null>(null);
  const [copied, setCopied] = useState(false);

  const solve = (v = text) => {
    if (!items) return;
    const h = matchError(items, v);
    setHits(h);
    setCopied(false);
    if (v !== initial) track(h.length ? "rescue_hit" : "rescue_miss", { hit: h[0]?.h ?? "", text: v.slice(0, 90) });
  };
  useEffect(() => { if (items) solve(initial); /* eslint-disable-next-line react-hooks/exhaustive-deps */ }, [items]);

  const best = hits?.[0];
  const say = best?.p ?? genericPrompt(text.trim());

  return (
    <div>
      <div className="chips" role="group" aria-label="试试这几条">
        {PRESETS.map((p) => (
          <button key={p.label} type="button" aria-pressed={text === p.text} onClick={() => { setText(p.text); solve(p.text); }}>{p.label}</button>
        ))}
      </div>
      <form className="ask" onSubmit={(e) => { e.preventDefault(); solve(); }}>
        <label htmlFor="errIn" className="sr">粘贴你的报错</label>
        <textarea id="errIn" value={text} rows={compact ? 2 : 3} spellCheck={false} autoComplete="off"
          placeholder="把终端里的红字整段原样贴进来"
          onChange={(e) => setText(e.target.value)} />
        <button type="submit">看人话解释</button>
      </form>

      <div className="answer" aria-live="polite">
        <div className="ans-a">
          <span className="lab">人话解释</span>
          {!items && <h3>正在载入急救手册……</h3>}
          {items && best && (<>
            <h3>{best.t}</h3>
            <p>{best.d}</p>
            <Link className="more" href={best.h}>看完整解法和自己动手的步骤 →</Link>
            {hits && hits.length > 1 && (
              <div className="alt-hits"><span>也可能是：</span>{hits.slice(1).map((h) => <Link key={h.h} href={h.h}>{h.t}</Link>)}</div>
            )}
          </>)}
          {items && !best && (<>
            <h3>这条还没收进手册，但办法是一样的。</h3>
            <p>把终端里的红字整段、原样复制，不要只挑一行；再告诉AI你当时在做什么。右边这段话已经帮你把要求写好了，粘过去就行。</p>
            <Link className="more" href="/fix">看全部急救条目 →</Link>
          </>)}
        </div>
        <div className="ans-b">
          <span className="lab">复制给AI的那段话</span>
          <p className="say">{say}</p>
          <button className={`copy${copied ? " done" : ""}`} type="button" onClick={() => { track("copy_prompt", { page: "rescue", hit: best?.h ?? "generic" }); copyText(say).then(() => setCopied(true)); }}>
            {copied ? "已复制，去粘给AI" : "复制这段话"}
          </button>
        </div>
      </div>
    </div>
  );
}
