"use client";
import Link from "next/link";
import { useEffect, useState } from "react";
import { matchError, type IndexItem } from "@/lib/match";
import { useIndex } from "./useIndex";
import { copyText } from "./copy";
import { track } from "./track";
import Mk from "./Mk";
import { COPY_SVG, OK_SVG, inlineCode } from "@/lib/term";

const PRESETS = [
  { label: "npm ERR! code EACCES", circle: "permission denied", text: "npm ERR! code EACCES\nnpm ERR! syscall mkdir\nnpm ERR! Error: EACCES: permission denied, mkdir '/usr/local/lib/node_modules'" },
  { label: "zsh: command not found: npm", circle: "command not found", text: "zsh: command not found: npm" },
  { label: "EADDRINUSE :::3000", circle: "address already in use", text: "Error: listen EADDRINUSE: address already in use :::3000" },
  { label: "blocked by CORS policy", circle: "blocked by CORS policy", text: "Access to fetch at 'https://api.example.com/data' from origin 'http://localhost:3000' has been blocked by CORS policy" },
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
  const [editing, setEditing] = useState(!PRESETS.some((p) => p.text === initial));
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

  const preset = PRESETS.find((p) => p.text === text);
  const best = hits?.[0];
  const say = best?.p ?? genericPrompt(text.trim());
  const sayLines = say.split("\n");

  return (
    <div>
      <div className="fix-chips" role="group" aria-label="试试这几条">
        <span className="cap">点一条试试：</span>
        {PRESETS.map((p) => (
          <button key={p.label} type="button" className="chip mono" aria-pressed={text === p.text} onClick={() => { setText(p.text); setEditing(false); solve(p.text); }}>{p.label}</button>
        ))}
      </div>
      <div className="fix-wrap">
        <div className="fix-l">
          <p className="stp"><i>1</i>红字原样贴进来</p>
          <form className="term err" onSubmit={(e) => { e.preventDefault(); solve(); }}>
            <div className="term-h">
              <b>你贴进来的</b><span className="tag err">终端报错</span><span className="sp" />
              <button type="button" className="tbtn" aria-pressed={editing} onClick={() => setEditing(!editing)}>{editing ? "用示例" : "贴我自己的"}</button>
            </div>
            {!editing && (
              <div className="term-b">
                {text.split("\n").map((l, i) => {
                  const at = preset ? l.indexOf(preset.circle) : -1;
                  return <div className="ln" key={`${text}-${i}`}><i>{i + 1}</i><span className="e">{at >= 0 ? <>{l.slice(0, at)}<Mk k="o">{preset!.circle}</Mk>{l.slice(at + preset!.circle.length)}</> : l}</span></div>;
                })}
                <div className="ln" style={{ marginTop: 18 }}><i>$</i><span className="dim"><span className="cursor" /></span></div>
              </div>
            )}
            {editing && <>
              <label htmlFor="errIn" className="sr">粘贴你的报错</label>
              <textarea id="errIn" className="err-in" value={text} rows={compact ? 4 : 6} spellCheck={false} autoComplete="off" autoFocus={false}
                placeholder="把终端里的红字整段粘贴到这里…" onChange={(e) => setText(e.target.value)} />
              <div className="term-f"><button type="submit" className="btn pri">看人话解释</button></div>
            </>}
          </form>
        </div>
        <div className="fix-r" aria-live="polite">
          <div>
            <p className="stp"><i>2</i>先给你一句人话</p>
            <div className="card human">
              {!items && <h3>正在载入急救手册……</h3>}
              {items && best && (<>
                <span className="pill">{editing ? "认出来了" : "人话"}</span>
                <h3>{best.t}</h3>
                <p>{best.d}</p>
                <Link href={best.h}>看完整解法和自己动手的步骤 →</Link>
                {hits && hits.length > 1 && <div className="alt-hits"><span className="cap">也可能是：</span>{hits.slice(1).map((h) => <Link key={h.h} href={h.h}>{h.t}</Link>)}</div>}
              </>)}
              {items && !best && (<>
                <span className="pill plain">手册里暂时没有这一条</span>
                <h3>先用通用的话问AI。</h3>
                <p className="nomatch">把终端里的红字整段、原样复制，不要只挑一行；再告诉AI你当时在做什么。下面这段已经帮你写好了，复制给AI就行。</p>
                <Link href="/fix">看全部急救条目 →</Link>
              </>)}
            </div>
          </div>
          <div>
            <p className="stp"><i>3</i>再复制这段话给AI</p>
            <div className="term prompt">
              <div className="term-h">
                <b>复制给AI</b><span className="tag">Claude Code / Codex / DeepSeek Harness</span><span className="sp" />
                <button type="button" className={`tbtn${copied ? " ok" : ""}`} onClick={() => { track("copy_prompt", { page: "rescue", hit: best?.h ?? "generic" }); copyText(say).then(() => setCopied(true)); }}>
                  <span dangerouslySetInnerHTML={{ __html: copied ? OK_SVG : COPY_SVG }} />{copied ? "已复制，去粘给AI" : "复制"}
                </button>
              </div>
              <div className="term-b">
                {sayLines.map((l, i) => <div className="ln" key={i}><i>{i + 1}</i><span dangerouslySetInnerHTML={{ __html: inlineCode(l) || "&#8203;" }} /></div>)}
              </div>
            </div>
          </div>
        </div>
      </div>
    </div>
  );
}
