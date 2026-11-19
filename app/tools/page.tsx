import type { Metadata } from "next";
import ToolPicker from "@/components/ToolPicker";
import { TOOLS, CAT_LABEL, VERIFIED, CHANGES, type Cat } from "@/lib/tools";

export const metadata: Metadata = {
  title: "AI编程工具怎么选：Claude Code、Codex、Cursor、TRAE对比（2026年11月）",
  description: "17款AI编程工具的免费额度、价格、能不能在国内用、适合谁，每条都附官方价格页和核实日期。回答三个问题，告诉你先试哪个。",
  alternates: { canonical: "/tools" },
};

const CN = { yes: ["ok", "国内能用"], no: ["no", "大陆不支持"], unknown: ["no", "请自行确认"] } as const;

export default function Page() {
  const cats: Cat[] = ["web", "ide", "cli"];
  return (<>
    <header className="sec-head white">
      <div className="wrap">
        <div><span className="lab">工具怎么选 · 核实于{VERIFIED}</span><h1>先选对工具，<br />再谈做东西。</h1></div>
        <p>AI编程工具分三类：网页里一句话生成、AI编辑器、命令行Agent。它们的价格和额度每隔几周就变，下面每一条都写了核实日期，并链到官方价格页——下单前以官网为准。</p>
      </div>
    </header>
    <div className="wrap">
      <ToolPicker />
      {cats.map((c) => (
        <section key={c} className="cat-block" id={c}>
          <h2>{CAT_LABEL[c]}<small>{TOOLS.filter((t) => t.cat === c).length}款</small></h2>
          <div className="tool-table-wrap">
            <table className="tool-table">
              <thead><tr><th>工具</th><th>它是什么</th><th>免费能用多少</th><th>付费</th><th>国内</th><th>适合谁 / 短板</th></tr></thead>
              <tbody>
                {TOOLS.filter((t) => t.cat === c).map((t) => (
                  <tr key={t.id} id={t.id}>
                    <td>{t.name}<small>{t.maker}</small></td>
                    <td>{t.what}</td>
                    <td>{t.free}</td>
                    <td>{t.paid}{t.confidence !== "high" && <><br /><span className="src">以官网为准</span></>}<br /><span className="src"><a href={t.pricing} target="_blank" rel="noopener">官方价格页</a></span></td>
                    <td><span className={`tag ${CN[t.cn][0]}`}>{CN[t.cn][1]}</span><br /><span className="src">{t.cnNote}</span></td>
                    <td>{t.bestFor}<br /><span className="src">短板：{t.weak}</span></td>
                  </tr>
                ))}
              </tbody>
            </table>
          </div>
        </section>
      ))}
      <section className="cat-block" id="changes">
        <h2>最近的变化<small>老教程最容易在这里过期</small></h2>
        <ul className="index-list" style={{ columns: 1 }}>
          {CHANGES.map((c) => (
            <li key={c.what}><a href={c.src} target="_blank" rel="noopener"><span className="t">{c.what}</span><span className="k">{c.date}</span></a></li>
          ))}
        </ul>
      </section>
      <div className="callout callout-warn" style={{ margin: "40px 0 80px", maxWidth: 820 }}>
        <b>关于「国内能不能用」</b>
        <p>Anthropic和OpenAI的官方受支持地区名单都不含中国大陆，所以Claude Code、Codex在大陆无法直接合规使用，名单外使用可能被封号。本站不教任何绕过地区限制的办法。没有海外网络和外币卡的话，走国内线一样能把八关走完：TRAE、Qoder、CodeBuddy、Kimi Code这些工具都能用人民币付费。</p>
      </div>
    </div>
  </>);
}
