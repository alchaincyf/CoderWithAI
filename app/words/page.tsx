import type { Metadata } from "next";
import GroupedIndex from "@/components/GroupedIndex";
import { getDocs } from "@/lib/content";

export const metadata: Metadata = {
  title: "人话词典：AI编程里的行话，翻成人话",
  description: "终端、API、部署、环境变量、Git、MCP……用AI编程时最常听到的词，每个都用一个比方讲清楚，并告诉你AI提到它时该怎么接。",
  alternates: { canonical: "/words" },
};

export default function Page() {
  const docs = getDocs("words");
  return (<>
    <header className="sec-head ink">
      <div className="wrap">
        <div><span className="lab">人话词典 · {docs.length}个词</span><h1>AI嘴里的行话，<br />翻成人话。</h1></div>
        <p>你不用学会写代码，但得听懂AI在说什么。每个词一页：一句话讲清楚、一个比方、你会在哪儿碰到它、AI提到它时你该怎么接。</p>
      </div>
    </header>
    <div className="wrap" style={{ paddingBottom: 80 }}>
      <GroupedIndex docs={docs} order={["基础", "网页", "上线", "数据", "连接", "版本管理", "AI", "App"]} tagOf={(d) => (d.data.level !== undefined ? `第${d.data.level}关` : undefined)} />
    </div>
  </>);
}
