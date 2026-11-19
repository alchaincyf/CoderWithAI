"use client";
import Link from "next/link";
import { useState } from "react";
import Mk from "./Mk";
import { copyText } from "./copy";
import { track } from "./track";
import { COPY_SVG, OK_SVG } from "@/lib/term";

export interface Teaser { name: string; bad: string; fix: string; when: string; rules: string[] }

// 提示词技巧：随口一说 → 改成这样
export function PromptTeaser({ items }: { items: Teaser[] }) {
  const [i, setI] = useState(0);
  const p = items[i];
  return (<>
    <div className="pt-chips" role="tablist" aria-label="选一个场景">
      {items.map((x, k) => <button key={x.name} type="button" role="tab" className="chip" aria-selected={k === i} onClick={() => setI(k)}>{x.name}</button>)}
    </div>
    <div className="pt" key={i}>
      <div className="bad"><span className="l">你随口一说</span><Mk k="s" wrap>{p.bad}</Mk></div>
      <div className="fx"><span className="l">改成这样</span><b>{p.fix}</b></div>
    </div>
    <p className="pt-n">什么时候用：{p.when}</p>
    <ol className="rules">{p.rules.map((r) => <li key={r}>{r}</li>)}</ol>
  </>);
}

// 需求说明书迷你版：填两个空，实时变出一段需求
export function MiniBrief() {
  const [a, setA] = useState("记账App");
  const [b, setB] = useState("");
  const [copied, setCopied] = useState(false);
  const ph = (t: string) => `（${t}）`;
  const L = [
    `我想做一个${a.trim() || ph("做什么")}，给${b.trim() || ph("谁")}用。`,
    `他们现在${ph("现状痛点")}，我想让他们${ph("想达到的效果")}。`,
    `先做这几件事：${ph("核心功能")}。`,
    `做成什么样算好：${ph("验收标准")}。`,
    "别急着写，先用你的话告诉我你准备怎么做，有不同方案就列出来我选。",
  ];
  const lit = (l: string) => l.split(/(（[^）]*）)/).map((s, k) => (s.startsWith("（") ? <span key={k} className="ph">{s}</span> : s));
  return (<>
    <div className="fld"><label htmlFor="b1">做什么</label><input id="b1" placeholder="记账App / 公司介绍网站" value={a} onChange={(e) => { setA(e.target.value); setCopied(false); }} /></div>
    <div className="fld"><label htmlFor="b2">给谁用</label><input id="b2" placeholder="不会记账的上班族" value={b} onChange={(e) => { setB(e.target.value); setCopied(false); }} /></div>
    <div className="term prompt">
      <div className="term-h"><b>生成的需求</b><span className="sp" />
        <button type="button" className={`tbtn${copied ? " ok" : ""}`} onClick={() => { track("copy_brief", { page: "home" }); copyText(L.join("\n")).then(() => setCopied(true)); }}>
          <span dangerouslySetInnerHTML={{ __html: copied ? OK_SVG : COPY_SVG }} />{copied ? "已复制" : "复制"}
        </button>
      </div>
      <div className="term-b">{L.map((l, k) => <div className="ln" key={k}><i>{k + 1}</i><span>{lit(l)}</span></div>)}</div>
    </div>
  </>);
}

// 人话词典：换一个词
export function WordPeek({ words }: { words: { title: string; summary: string; href: string }[] }) {
  const [i, setI] = useState(0);
  const w = words[i];
  return (<>
    <div className="wd"><b><Link href={w.href}>{w.title}</Link></b><span>{w.summary}</span></div>
    <button type="button" className="btn" style={{ alignSelf: "flex-start" }} onClick={() => setI((i + 1) % words.length)}>换一个词</button>
  </>);
}
