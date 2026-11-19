"use client";
import { useState } from "react";
import { TOOLS, CAT_LABEL } from "@/lib/tools";

type A = { net?: "yes" | "no"; how?: "web" | "ide" | "cli"; budget?: "free" | "mid" | "high" };

function recommend(a: A) {
  const picks: { id: string; why: string }[] = [];
  const cn = a.net !== "yes";
  if (cn) {
    if (a.how === "web") picks.push({ id: "qwenwork", why: "国内直接用，说话就能做出交互网页并发布，免费档就能试" }, { id: "deepseek-harness", why: "想更进一步、把项目握在自己手里，就换到它：在浏览器里操作，人民币按量付费" });
    else if (a.how === "cli") picks.push({ id: "deepseek-harness", why: "国内最推荐：DeepSeek官方出品，不用海外网络，用法和Claude Code一个路子" }, { id: "cc-switch", why: "想用Claude Code或Codex本身？装上它，把模型切到DeepSeek、Kimi、GLM的官方API" });
    else picks.push({ id: "trae-cn", why: "免费版每月500积分，中文界面，在编辑器里看着AI改" }, { id: "deepseek-harness", why: "想让AI全权干活时换到它，国内最推荐的AI编程Agent" });
  } else {
    if (a.how === "web") picks.push({ id: "lovable", why: "一句话做出能上线的网站，免费档够你做出第一个东西" }, { id: "claude-code", why: "网页工具做到一半不够用了，就该换到这个，项目能完全掌握在自己手里" });
    else if (a.how === "cli") picks.push({ id: "claude-code", why: "AI编程Agent的标杆，教程和插件生态最多，本站路线默认用它" }, { id: "codex", why: "和Claude Code同一档，已经订了ChatGPT就不用再花钱，手机上也能派活" });
    else picks.push({ id: "cursor", why: "最多人用的AI编辑器，VS Code手感" }, { id: "claude-code", why: "想让AI替你把整件事做完，就从编辑器升级到它" });
  }
  if (a.budget === "free" && !cn && a.how === "cli") picks.push({ id: "antigravity", why: "部分推荐：有免费档，适合先试试Agent式编程，但额度和模型变动频繁" });
  if (a.budget === "free") return picks.map((p) => ({ ...p, why: p.why + "。先用免费档或少量充值，确定离不开再加钱" }));
  return picks;
}

const Q = <T extends string>({ label, value, opts, onPick }: { label: string; value?: T; opts: [T, string][]; onPick: (v: T) => void }) => (
  <div className="q"><b>{label}</b><div className="opts">{opts.map(([v, t]) => <button key={v} type="button" aria-pressed={value === v} onClick={() => onPick(v)}>{t}</button>)}</div></div>
);

export default function ToolPicker() {
  const [a, setA] = useState<A>({ net: "no", how: "ide", budget: "free" });
  const picks = recommend(a);
  return (
    <div className="picker">
      <div className="picker-q">
        <h2>回答三个问题</h2>
        <Q label="1. 你能稳定使用海外网络，并且有外币卡付款吗？" value={a.net} onPick={(net) => setA({ ...a, net })} opts={[["no", "不能／不确定"], ["yes", "能"]]} />
        <Q label="2. 你更想怎么做？" value={a.how} onPick={(how) => setA({ ...a, how })} opts={[["web", "在网页里说话，直接出成品"], ["ide", "在编辑器里看着它改"], ["cli", "在终端里让AI全权干活"]]} />
        <Q label="3. 预算？" value={a.budget} onPick={(budget) => setA({ ...a, budget })} opts={[["free", "先免费"], ["mid", "每月一两百元内"], ["high", "能为效率多花"]]} />
        <p className="note" style={{ marginTop: 22 }}>不知道选什么？第0关讲了三类工具的区别：<a href="/start/what-is-ai-coding">先搞懂AI编程是怎么回事</a>。</p>
      </div>
      <div className="picker-a" aria-live="polite">
        <span className="lab">建议你先试</span>
        {picks.map((p) => {
          const t = TOOLS.find((x) => x.id === p.id)!;
          return (
            <div className="pick" key={p.id}>
              <h3>{t.name}<small>{CAT_LABEL[t.cat]}</small></h3>
              <p>{p.why}。</p>
              <p className="note">免费：{t.free}　付费：{t.paid}　<a href={t.pricing} target="_blank" rel="noopener">官方价格页</a></p>
            </div>
          );
        })}
      </div>
    </div>
  );
}
