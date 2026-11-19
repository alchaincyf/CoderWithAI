"use client";
import { useEffect, useRef, useState } from "react";

type K = "water" | "timer" | "lots";
const EX: Record<K, { tab: string; url: string; need: string; steps: string[] }> = {
  water: {
    tab: "喝水记录", url: "water.example",
    need: "帮我做一个记录每天喝了几杯水的小网页，点一下加一杯，满8杯放个烟花。",
    steps: ["新建 index.html", "写好「点一下加一杯」", "写好「满8杯放烟花」"],
  },
  timer: {
    tab: "番茄钟", url: "focus.example",
    need: "帮我做一个25分钟的专注倒计时，能开始、暂停，时间到了提醒我休息。",
    steps: ["新建 index.html", "写好开始、暂停、重来", "写好「时间到」的提示"],
  },
  lots: {
    tab: "抽签小工具", url: "lunch.example",
    need: "帮我做一个抽签小工具，我填几个选项，点一下随机抽一个，中午吃什么交给它。",
    steps: ["新建 index.html", "写好「填选项」的输入框", "写好「点一下随机抽」"],
  },
};
const COLORS = ["#1d36c8", "#ff5a1f", "#f6e84a", "#101010"];

function useBurst() {
  const host = useRef<HTMLDivElement>(null);
  const fire = () => {
    const w = host.current; if (!w) return;
    const b = document.createElement("div"); b.className = "burst";
    for (let i = 0; i < 30; i++) {
      const s = document.createElement("span"), a = Math.random() * Math.PI * 2, d = 70 + Math.random() * 120;
      s.style.background = COLORS[i % 4];
      s.style.setProperty("--dx", Math.cos(a) * d + "px"); s.style.setProperty("--dy", Math.sin(a) * d + "px");
      if (i % 3 === 0) s.style.borderRadius = "50%";
      b.appendChild(s);
    }
    w.appendChild(b); setTimeout(() => b.remove(), 1000);
  };
  return { host, fire };
}

function Water({ fire }: { fire: () => void }) {
  const [n, setN] = useState(0);
  return (<>
    <div className="w-title">今天喝水</div>
    <div className="w-big">{n}<small> / 8 杯</small></div>
    <div className="glasses">{Array.from({ length: 8 }, (_, i) => <div key={i} className={`glass${i < n ? " on" : ""}`} />)}</div>
    <div className="row">
      <button className="wb main" type="button" onClick={() => { if (n < 8) { setN(n + 1); if (n + 1 === 8) fire(); } }}>加一杯</button>
      <button className="wb" type="button" onClick={() => setN(Math.max(0, n - 1))}>少一杯</button>
      <button className="wb" type="button" onClick={() => setN(0)}>重来</button>
    </div>
    <p className="msg">{n >= 8 ? "满8杯，今天够了。" : n ? `再来 ${8 - n} 杯。` : "点「加一杯」试试。"}</p>
  </>);
}

function Timer({ fire }: { fire: () => void }) {
  const [mode, setMode] = useState<"focus" | "rest">("focus");
  const [left, setLeft] = useState(1500);
  const [run, setRun] = useState(false);
  const [msg, setMsg] = useState("点「开始」，倒计时就走起来。");
  useEffect(() => {
    if (!run) return;
    const id = setInterval(() => setLeft((l) => {
      if (l <= 1) { setRun(false); setMsg(mode === "focus" ? "时间到，站起来走走。" : "休息结束，回来干活。"); fire(); return 0; }
      return l - 1;
    }), 1000);
    return () => clearInterval(id);
  }, [run, mode, fire]);
  const mm = String(Math.floor(left / 60)).padStart(2, "0"), ss = String(left % 60).padStart(2, "0");
  return (<>
    <div className="w-title">{mode === "focus" ? "专注中" : "休息一下"}</div>
    <div className="w-big">{mm}:{ss}</div>
    <div className="row">
      <button className="wb main" type="button" onClick={() => setRun(!run)}>{run ? "暂停" : "开始"}</button>
      <button className="wb" type="button" onClick={() => { setRun(false); setLeft(mode === "focus" ? 1500 : 300); setMsg("已重来。"); }}>重来</button>
      <button className="wb" type="button" onClick={() => { const m = mode === "focus" ? "rest" : "focus"; setRun(false); setMode(m); setLeft(m === "focus" ? 1500 : 300); setMsg(""); }}>
        {mode === "focus" ? "切到休息5分钟" : "切回专注25分钟"}
      </button>
    </div>
    <p className="msg">{msg}</p>
  </>);
}

function Lots({ fire }: { fire: () => void }) {
  const [opts, setOpts] = useState("火锅，麻辣烫，沙县小吃，楼下面馆");
  const [out, setOut] = useState("？");
  const busy = useRef(false);
  const draw = () => {
    if (busy.current) return;
    const arr = opts.split(/[,，、\s]+/).filter(Boolean);
    if (arr.length < 2) { setOut("至少填两个"); return; }
    busy.current = true; let k = 0;
    const t = setInterval(() => {
      setOut(arr[Math.floor(Math.random() * arr.length)]);
      if (++k >= 14) { clearInterval(t); busy.current = false; fire(); }
    }, 70);
  };
  return (<>
    <div className="w-title">中午吃什么</div>
    <input className="lot-in" value={opts} onChange={(e) => setOpts(e.target.value)} aria-label="选项，用逗号隔开" />
    <div className="lot-out">{out}</div>
    <div className="row"><button className="wb main" type="button" onClick={draw}>抽一签</button></div>
    <p className="msg">选项可以改，用逗号隔开。</p>
  </>);
}

export default function HeroDemo() {
  const [k, setK] = useState<K>("water");
  const { host, fire } = useBurst();
  const e = EX[k];
  return (<>
    <div className="cover-l">
      <div className="tabs" role="tablist" aria-label="换一个例子">
        {(Object.keys(EX) as K[]).map((key) => (
          <button key={key} role="tab" type="button" aria-selected={k === key} onClick={() => setK(key)}>{EX[key].tab}</button>
        ))}
      </div>
      <div className="need"><span className="lab">你只说了一句人话</span><p>{e.need}</p></div>
    </div>
    <div className="stage-slot" data-demo-stage>
      <div className="win browser">
        <div className="win-top"><i /><i /><i /><span className="url">{e.url}</span></div>
        <div className="widget" ref={host}>
          {k === "water" && <Water fire={fire} />}
          {k === "timer" && <Timer fire={fire} />}
          {k === "lots" && <Lots fire={fire} />}
        </div>
      </div>
      <div className="win term" aria-hidden="true">
        <div className="win-top"><i /><i /><i /><span>AI编程工具 · 示意</span></div>
        <pre>
          <span className="u">&gt; {e.need.slice(0, 18)}…</span>{"\n"}
          {e.steps.map((s) => <span key={s} className="dim">● {s}{"\n"}</span>)}
          <span className="ok">✓ 做好了，打开看看</span>
        </pre>
      </div>
    </div>
  </>);
}
