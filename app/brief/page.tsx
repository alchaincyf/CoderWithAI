import type { Metadata } from "next";
import PageHead from "@/components/PageHead";
import Mk from "@/components/Mk";
import BriefBuilder from "@/components/BriefBuilder";

export const metadata: Metadata = {
  title: "需求说明书生成器：把想法整理成AI一读就懂的需求",
  description: "不会写需求文档？填几个空，生成一份结构清楚的需求说明，直接发给Claude Code、Codex、Cursor、TRAE。包括验收标准和让AI先复述再动手的做事方式。",
  alternates: { canonical: "/brief" },
};

export default function Page() {
  return (<>
    <PageHead kicker="需求说明书生成器" title={<>AI做得好不好，<br /><Mk k="u">一半</Mk>看你怎么说。</>}>
      同样一句「帮我做个记账App」，AI会做出十种东西。把下面几个空填上，右边会生成一份结构清楚的需求说明——包括最容易被忽略的两栏：「明确不要什么」和「怎么算做完」。
    </PageHead>
    <div className="wrap wide"><BriefBuilder /></div>
  </>);
}
