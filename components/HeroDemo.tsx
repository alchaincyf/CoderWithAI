"use client";
import { useEffect, useRef, useState } from "react";
import Mk from "./Mk";
import { burstAt } from "./burst";

type K = 0 | 1 | 2;
const G = ({ k, n, children }: { k: "o" | "u"; n: number; children: string }) => (
  <span className="g"><Mk k={k}>{children}</Mk><span className="nm">{n}</span></span>
);

const EX: { tab: string; sent: React.ReactNode; notes: string[]; log: string[]; first: string }[] = [
  {
    tab: "喝水记录", first: "帮我做一个记录每天喝了几杯水的小网页",
    sent: <>帮我做一个<G k="o" n={1}>记录每天喝了几杯水</G>的小网页，<G k="u" n={2}>点一下加一杯</G>，<G k="o" n={3}>满8杯</G>放个烟花。</>,
    notes: ["做什么：只说一件事，而且具体。", "怎么用：「点一下加一杯」，AI才知道交互长什么样。", "做到什么程度：满8杯放烟花，AI才知道在哪里停。"],
    log: ["新建 index.html", "写好「点一下加一杯」", "写好「满8杯放烟花」"],
  },
  {
    tab: "番茄钟", first: "做个番茄钟，25分钟专注，到点提醒我歇一会",
    sent: <>做个<G k="o" n={1}>番茄钟</G>，<G k="u" n={2}>25分钟专注</G>，<G k="o" n={3}>到点提醒我歇一会</G>。</>,
    notes: ["做什么：一个词就够，AI知道番茄钟长什么样。", "多长：写出数字，别让AI替你猜。", "做到什么程度：到点要有提醒，才算做完。"],
    log: ["新建 index.html", "写好倒计时圆环", "写好「专注25／休息5」切换"],
  },
  {
    tab: "抽签小工具", first: "做个抽签小工具，我写几个选项，它帮我挑一个",
    sent: <>做个<G k="o" n={1}>抽签小工具</G>，<G k="u" n={2}>我写几个选项</G>，<G k="u" n={3}>它帮我挑一个</G>。</>,
    notes: ["做什么：抽签，一个词定下形态。", "输入：选项由你来写，AI就会留一个输入框。", "输出：只要一个结果，别让它给你列一堆。"],
    log: ["新建 index.html", "写好选项输入框", "写好「随机挑一个」"],
  },
];

function Water() {
  const [n, setN] = useState(0);
  const add = useRef<HTMLButtonElement>(null);
  return (<>
    <div className="cups">{Array.from({ length: 8 }, (_, k) => <div key={k} className={`cup${k < n ? " f" : ""}`}><i /><span>{k + 1}</span></div>)}</div>
    <div className="w-num"><b>{n}</b><small>/ 8 杯</small></div>
    <p className="w-msg" aria-live="polite">{n === 0 ? "点「加一杯」试试。" : n >= 8 ? "满8杯，今天喝够了。" : `再来 ${8 - n} 杯。`}</p>
    <div className="w-btns">
      <button type="button" className="btn pri" ref={add} onClick={() => { if (n < 8) { setN(n + 1); if (n + 1 === 8) burstAt(add.current); } }}>加一杯</button>
      <button type="button" className="btn" onClick={() => setN(Math.max(0, n - 1))}>少一杯</button>
      <button type="button" className="btn ghost" onClick={() => setN(0)}>重来</button>
    </div>
  </>);
}

function Timer() {
  const [mode, setMode] = useState<25 | 5>(25);
  const [left, setLeft] = useState(25 * 60);
  const [run, setRun] = useState(false);
  const [msg, setMsg] = useState("点「开始」，到点会提醒你。");
  const go = useRef<HTMLButtonElement>(null);
  const C = 2 * Math.PI * 74;
  useEffect(() => {
    if (!run) return;
    const id = setInterval(() => setLeft((l) => {
      if (l <= 1) { setRun(false); setMsg("时间到，歇一会。"); burstAt(go.current, 18); return 0; }
      return l - 1;
    }), 1000);
    return () => clearInterval(id);
  }, [run]);
  const pick = (m: 25 | 5) => { setMode(m); setRun(false); setLeft(m * 60); setMsg("点「开始」，到点会提醒你。"); };
  const mm = String(Math.floor(left / 60)).padStart(2, "0"), ss = String(left % 60).padStart(2, "0");
  return (<>
    <div className="ring">
      <svg viewBox="0 0 168 168" aria-hidden="true"><circle className="bg" cx="84" cy="84" r="74" /><circle className="fg" cx="84" cy="84" r="74" strokeDasharray={C} strokeDashoffset={C * (1 - left / (mode * 60))} /></svg>
      <div className="t" role="timer" aria-live="off">{mm}:{ss}</div>
    </div>
    <div className="seg" role="group" aria-label="专注或休息">
      <button type="button" aria-pressed={mode === 25} onClick={() => pick(25)}>专注25</button>
      <button type="button" aria-pressed={mode === 5} onClick={() => pick(5)}>休息5</button>
    </div>
    <p className="w-msg" aria-live="polite">{run ? (mode === 25 ? "专注中，别看手机。" : "休息一下，喝口水。") : msg}</p>
    <div className="w-btns" style={{ justifyContent: "center" }}>
      <button type="button" className="btn pri" ref={go} onClick={() => { if (left === 0) setLeft(mode * 60); setRun(!run); }}>{run ? "暂停" : "开始"}</button>
      <button type="button" className="btn" onClick={() => pick(mode)}>重置</button>
    </div>
  </>);
}

function Lots() {
  const [opts, setOpts] = useState("火锅 烧烤 面条 沙拉");
  const [out, setOut] = useState("今晚吃什么？");
  const [spin, setSpin] = useState(false);
  const busy = useRef(false);
  const draw = () => {
    const arr = opts.split(/[\s,，、]+/).filter(Boolean);
    if (!arr.length || busy.current) return;
    busy.current = true; setSpin(true);
    let k = 0;
    const t = setInterval(() => {
      setOut(arr[Math.floor(Math.random() * arr.length)]);
      if (++k > 9) { clearInterval(t); busy.current = false; setSpin(false); }
    }, 70);
  };
  return (<>
    <div className="lot"><label className="cap" htmlFor="lt">选项，用空格隔开</label><textarea id="lt" rows={2} value={opts} onChange={(e) => setOpts(e.target.value)} /></div>
    <div className={`lot-res${spin ? " spin" : ""}`} aria-live="polite">{out}</div>
    <button type="button" className="btn pri" style={{ alignSelf: "stretch" }} onClick={draw}>帮我挑一个</button>
  </>);
}

export default function HeroDemo() {
  const [k, setK] = useState<K>(0);
  const [shown, setShown] = useState(0);
  const e = EX[k];
  const total = e.log.length + 2; // 提示行 + 步骤 + 完成
  useEffect(() => {
    setShown(0);
    const reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    if (reduce) { setShown(total); return; }
    const ids = Array.from({ length: total }, (_, i) => setTimeout(() => setShown(i + 1), 350 + i * 520));
    return () => ids.forEach(clearTimeout);
  }, [k, total]);

  return (
    <div className="bench card" id="bench">
      <div className="bench-h">
        <div className="seg" role="tablist" aria-label="换一个例子">
          {EX.map((x, i) => <button key={x.tab} type="button" role="tab" aria-selected={k === i} onClick={() => setK(i as K)}>{x.tab}</button>)}
        </div>
        <span className="cap">左边是你说的一句话，右边是AI做出来的，真的能点</span>
      </div>
      <div className="bench-body">
        <section className="b-left" aria-label="你说的话和朱笔拆解">
          <div className="b-say" key={k}>
            <div><p className="cap">你只说了一句人话</p><p className="sent">{e.sent}</p></div>
            <div><p className="cap">朱笔：这句话里藏着三件事</p>
              <ol className="notes">{e.notes.map((t, i) => { const [a, ...b] = t.split("："); return <li key={i}><span className="nm">{i + 1}</span><span><b>{a}</b>：{b.join("：")}</span></li>; })}</ol>
            </div>
          </div>
          <div className="term log" aria-hidden="true">
            <div className="term-h"><b>AI编程工具</b><span className="tag">示意</span><span className="sp" /><span className="dim">{shown >= total ? "完成" : "写代码中…"}</span></div>
            <div className="term-b">
              {[<><span className="dim">&gt;</span> {e.first}</>, ...e.log.map((t) => <><span className="ok">●</span> {t}</>), <><span className="ok">✓</span> 做好了，打开看看</>].map((l, i) => (
                <div key={`${k}-${i}`} className={`ln${i < shown ? " show" : ""}`}><i>{i + 1}</i><span>{l}</span></div>
              ))}
            </div>
          </div>
        </section>
        <section className="b-right" aria-label="AI做出来的东西">
          <p className="cap"><span className="pill">真的能点</span> AI做出来的</p>
          <div className={`wg${k === 1 ? " wg-c" : ""}`} key={k}>
            {k === 0 && <Water />}{k === 1 && <Timer />}{k === 2 && <Lots />}
          </div>
        </section>
      </div>
    </div>
  );
}
