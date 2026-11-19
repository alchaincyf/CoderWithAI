import type { Metadata } from "next";
import Link from "next/link";
import RouteList from "@/components/RouteList";
import ProgressStrip from "@/components/ProgressStrip";
import { getDocs } from "@/lib/content";

export const metadata: Metadata = {
  title: "闯关路线：不会写代码，八关用AI做出能上线、能收钱的作品",
  description: "从搞懂AI编程是怎么回事，到装好工具、做出第一个网页、上线、接数据库、接大模型、做成App或小程序、收到第一笔钱。每一关都有过关标准和可复制的提示词。",
  alternates: { canonical: "/start" },
};

export default function Page() {
  const times: Record<string, string> = {};
  for (const d of getDocs("path")) if (d.data.time) times[d.slug] = String(d.data.time);
  return (<>
    <header className="sec-head blue">
      <div className="wrap">
        <div><span className="lab">闯关路线 · 八关</span><h1>从一句话，<br />走到有人付费。</h1>
          <ProgressStrip />
        </div>
        <p>每一关只解决一件事，有明确的过关标准。前三关做完，你就有了一个发给朋友能打开的网址。进度存在你自己的浏览器里，不用注册。</p>
      </div>
    </header>
    <div className="wrap">
      <RouteList times={times} />
      <p className="note" style={{ margin: "-48px 0 80px" }}>想先看看能做出什么？去 <Link href="/projects">实战项目</Link> 挑一个。</p>
    </div>
  </>);
}
