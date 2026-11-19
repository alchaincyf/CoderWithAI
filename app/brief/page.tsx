import type { Metadata } from "next";
import BriefBuilder from "@/components/BriefBuilder";

export const metadata: Metadata = {
  title: "需求说明书生成器：把想法整理成AI一读就懂的需求",
  description: "不会写需求文档？填几个空，生成一份结构清楚的需求说明，直接发给Claude Code、Codex、Cursor、TRAE。包括验收标准和让AI先复述再动手的做事方式。",
  alternates: { canonical: "/brief" },
};

export default function Page() {
  return (<>
    <header className="sec-head blue">
      <div className="wrap">
        <div><span className="lab">需求说明书生成器</span><h1>AI做得好不好，<br />一半看你怎么说。</h1></div>
        <p>同样一句「帮我做个记账App」，AI会做出十种东西。把下面几个空填上，右边会生成一份结构清楚的需求说明——包括最容易被忽略的两栏：「明确不要什么」和「怎么算做完」。</p>
      </div>
    </header>
    <div className="wrap"><BriefBuilder /></div>
  </>);
}
