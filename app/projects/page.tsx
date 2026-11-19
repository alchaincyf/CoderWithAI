import type { Metadata } from "next";
import Link from "next/link";
import { getDocs, hrefOf } from "@/lib/content";

export const metadata: Metadata = {
  title: "实战项目：照着把提示词发给AI，做出真东西",
  description: "个人主页、电子请柬、习惯打卡、小游戏、Excel合并脚本、记账本、AI聊天机器人、浏览器插件……每个项目都是一串能直接复制给AI的提示词。",
  alternates: { canonical: "/projects" },
};
const DIFF = ["", "入门", "进阶", "挑战"];

export default function Page() {
  const docs = getDocs("recipes");
  return (<>
    <header className="sec-head lemon">
      <div className="wrap">
        <div><span className="lab">实战项目 · {docs.length}个</span><h1>挑一个，<br />照着做出来。</h1></div>
        <p>每个项目是一串按顺序发给AI的提示词：第一条就能跑出可用的第一版，后面每一条加一点东西。写清了要花多少时间、多少钱、卡住了去哪查。</p>
      </div>
    </header>
    <div className="wrap">
      <div className="recipe-grid">
        {docs.map((d, i) => (
          <Link key={d.slug} href={hrefOf(d)} className={`recipe${i === 0 ? " feature" : ""}`}>
            <div className="meta"><span>{DIFF[Number(d.data.difficulty)] ?? ""}</span><span>{String(d.data.time ?? "")}</span><span>{String(d.data.cost ?? "")}</span></div>
            <h3>{d.title}</h3>
            <p>{d.summary}</p>
            <span className="go">看做法 →</span>
          </Link>
        ))}
      </div>
    </div>
  </>);
}
