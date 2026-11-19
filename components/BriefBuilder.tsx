"use client";
import { useEffect, useMemo, useState } from "react";
import { copyText } from "./copy";
import { track } from "./track";

const INIT = {
  name: "家庭菜谱本",
  what: "一个记录我家常做的菜的小网站，能按食材找菜",
  who: "我和我妈，主要在手机上看",
  now: "现在菜谱散落在微信收藏和小红书里，做饭时翻半天找不到",
  must: "1. 添加一道菜：名字、照片、食材、步骤\n2. 按食材搜索：输入「土豆」能找到所有用土豆的菜\n3. 随机推荐今晚吃什么",
  not: "不要登录注册，不要评论，不要广告",
  look: "像一本干净的手写菜谱，暖色，大字，手机上单手好操作",
  where: "手机浏览器",
  data: "local",
  done: "我在手机上添加3道菜，关掉浏览器再打开还在；搜「土豆」能找到其中含土豆的那道",
};
type F = typeof INIT;
const DATA: Record<string, string> = {
  none: "不需要保存数据",
  local: "数据存在用户自己的浏览器里（localStorage）就够，不需要服务器和登录",
  cloud: "需要登录，数据存在云端数据库，换设备也能看到（请推荐最简单的方案，比如Supabase，并先告诉我需要我去注册什么）",
};
const KEY = "cwa-brief";

function build(f: F) {
  return `# 项目需求：${f.name || "（项目名）"}

## 背景
我不会写代码，需要你作为全栈工程师带着我把它做出来。请用大白话和我沟通，遇到需要我决定的事情先停下来问我。

## 这是什么
${f.what}

## 给谁用、在哪用
${f.who}。主要在${f.where}里使用。

## 现在的痛点
${f.now}

## 第一版必须有的功能
${f.must}

## 第一版明确不要
${f.not}

## 长什么样
${f.look}

## 数据
${DATA[f.data]}

## 怎么算做完（验收标准）
${f.done}

## 做事方式
1. 先不要写代码。用5–8句话复述你理解的需求，列出你打算怎么做（技术选择、文件结构、分几步），以及你需要我提供或决定的东西，等我确认。
2. 确认后再一步一步做，每做完一步告诉我怎么看到效果。
3. 优先选最简单、最少依赖、不需要付费服务的方案。
4. 做完后自己按上面的验收标准检查一遍，再告诉我结果。
5. 在项目根目录建一个 AGENTS.md，把这份需求的要点和我们约定的规矩记进去，以后每次开工先读它。`;
}

export default function BriefBuilder() {
  const [f, setF] = useState<F>(INIT);
  const [copied, setCopied] = useState(false);
  useEffect(() => { try { const s = localStorage.getItem(KEY); if (s) setF({ ...INIT, ...JSON.parse(s) }); } catch {} }, []);
  useEffect(() => { try { localStorage.setItem(KEY, JSON.stringify(f)); } catch {} setCopied(false); }, [f]);
  const out = useMemo(() => build(f), [f]);
  const set = (k: keyof F) => (e: React.ChangeEvent<HTMLInputElement | HTMLTextAreaElement | HTMLSelectElement>) => setF({ ...f, [k]: e.target.value });
  const T = ({ k, label, hint, rows = 2 }: { k: keyof F; label: string; hint: string; rows?: number }) => (
    <label>{label}<small>{hint}</small><textarea rows={rows} value={f[k]} onChange={set(k)} /></label>
  );
  return (
    <div className="brief">
      <form className="brief-form" onSubmit={(e) => e.preventDefault()}>
        <label>项目叫什么<small>随便起，以后能改</small><input value={f.name} onChange={set("name")} /></label>
        {T({ k: "what", label: "一句话说它是什么", hint: "别写「一个平台」，写它具体帮谁做成什么事" })}
        {T({ k: "who", label: "给谁用", hint: "越具体越好：「我和我妈」比「家庭用户」好十倍" })}
        {T({ k: "now", label: "他们现在怎么凑合的", hint: "痛点讲清楚，AI才知道什么最重要" })}
        {T({ k: "must", label: "第一版必须有的功能（3个以内）", hint: "第一版越小越容易成。每条写清楚「做什么、看到什么」", rows: 4 })}
        {T({ k: "not", label: "第一版明确不要什么", hint: "这一条最容易被忽略，也最能防止AI自作主张加一堆东西" })}
        {T({ k: "look", label: "长什么样", hint: "给参考：像哪个App、什么颜色、什么感觉" })}
        <label>主要在哪里用<select value={f.where} onChange={set("where")}><option>手机浏览器</option><option>电脑浏览器</option><option>微信里打开</option><option>电脑上运行的小脚本</option></select></label>
        <label>数据要不要保存<select value={f.data} onChange={set("data")}><option value="none">不用保存</option><option value="local">存在本机浏览器就行</option><option value="cloud">要登录，换设备也在</option></select></label>
        {T({ k: "done", label: "怎么算做完", hint: "写一个你能亲手检查的场景，这是最重要的一栏", rows: 3 })}
      </form>
      <div className="brief-out">
        <span className="lab">生成的需求说明（直接发给AI）</span>
        <pre>{out}</pre>
        <div className="row">
          <button className={`copy${copied ? " done" : ""}`} type="button" onClick={() => { track("copy_brief"); copyText(out).then(() => setCopied(true)); }}>{copied ? "已复制，去粘给AI" : "复制全部"}</button>
          <button className="copy" style={{ background: "transparent", color: "#f4f3ee", borderColor: "#f4f3ee" }} type="button" onClick={() => setF(INIT)}>恢复示例</button>
        </div>
        <p className="note" style={{ color: "#bdbcb6", marginTop: 14 }}>你填的内容只存在这台设备的浏览器里，不会上传。</p>
      </div>
    </div>
  );
}
