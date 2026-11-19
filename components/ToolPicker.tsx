"use client";
import { useState } from "react";
import { TOOLS, CAT_LABEL } from "@/lib/tools";

type A = { net?: "yes" | "no"; how?: "web" | "ide" | "cli"; budget?: "free" | "mid" | "high" };

function recommend(a: A) {
  const picks: { id: string; why: string }[] = [];
  const cn = a.net !== "yes";
  if (a.how === "web") {
    if (cn) picks.push({ id: "coze", why: "国内能直接用，说话就能做出网页和小应用，最快看到成品" }, { id: "miaoda", why: "百度的同类产品，可以免费体验，适合先试试手感" });
    else picks.push({ id: "lovable", why: "一句话做出能上线的网站，免费档够你做出第一个东西" }, { id: "bolt", why: "同类里额度给得比较大方，适合快速做原型" });
    picks.push({ id: cn ? "trae-cn" : "claude-code", why: "网页工具做到一半不够用了，就该换到这个——项目能完全掌握在自己手里" });
  } else if (a.how === "cli") {
    if (cn) picks.push({ id: "kimi-code", why: "国内网络能用的命令行Agent，体验最接近Claude Code" }, { id: "qwen-code", why: "开源，可以接国内任意一家模型" });
    else picks.push({ id: "claude-code", why: "命令行Agent的标杆，教程和插件生态最多，本站路线默认用它" }, { id: "codex", why: "已经订了ChatGPT就不用再花钱，手机上也能派活（同样要求账号所在地区受支持）" });
  } else {
    if (cn) picks.push({ id: "trae-cn", why: "免费版每月500积分、全功能可用，中文界面，国内新手首选" }, { id: "qoder-cn", why: "IDE加手机加命令行一套，想要全家桶可以试" }, { id: "codebuddy", why: "腾讯出品，微信扫码就能登录，免费额度可以先上手" });
    else picks.push({ id: "cursor", why: "最多人用的AI编辑器，VS Code手感" }, { id: "copilot", why: "最便宜的付费入口，10美元/月" });
  }
  if (a.budget === "free") return picks.map((p) => ({ ...p, why: p.why + "。先用免费档，确定离不开再付费。" }));
  if (a.budget === "high" && !cn && a.how !== "cli") picks.push({ id: "claude-code", why: "预算够的话，命令行Agent能替你把整件事做完，长期看最省时间" });
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
