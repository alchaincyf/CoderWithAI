import type { Metadata } from "next";
import Link from "next/link";
import PageHead from "@/components/PageHead";
import Mk from "@/components/Mk";
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
    <PageHead kicker={`实战项目 · ${docs.length}个`} title={<>挑一个，<br />照着<Mk k="u">做出来</Mk>。</>}>
      每个项目是一串按顺序发给AI的提示词：第一条就能跑出可用的第一版，后面每一条加一点东西。写清了要花多少时间、多少钱、卡住了去哪查。
    </PageHead>
    <div className="wrap wide">
      <div className="recipe-grid">
        {docs.map((d) => (
          <Link key={d.slug} href={hrefOf(d)} className="card recipe">
            <div className="meta">{[DIFF[Number(d.data.difficulty)], String(d.data.time ?? ""), String(d.data.cost ?? "")].filter(Boolean).map((m) => <span key={m} className="pill plain">{m}</span>)}</div>
            <h3>{d.title}</h3>
            <p>{d.summary}</p>
            <span className="go">看做法</span>
          </Link>
        ))}
      </div>
    </div>
  </>);
}
