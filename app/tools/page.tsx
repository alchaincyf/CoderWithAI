import type { Metadata } from "next";
import ToolPicker from "@/components/ToolPicker";
import { TOOLS, CAT_LABEL, RANK_LABEL, VERIFIED, CHANGES, type Cat, type Rank } from "@/lib/tools";

export const metadata: Metadata = {
  title: "AI编程工具怎么选：Claude Code、Codex、DeepSeek Harness对比，国内怎么用（2026年11月）",
  description: `${TOOLS.length}款AI编程工具的免费额度、价格、能不能在国内用、适合谁，每条都附官方价格页和核实日期。最推荐Claude Code和Codex，国内最推荐DeepSeek Harness，国内用CC Switch接国产模型。`,
  alternates: { canonical: "/tools" },
};

const CN = { yes: ["ok", "国内能用"], no: ["no", "大陆不支持"], unknown: ["no", "请自行确认"] } as const;

export default function Page() {
  const cats: Cat[] = ["cli", "switch", "ide", "office", "web"];
  const ranks: Rank[] = ["top", "cn-top", "cn", "intl", "partial", "careful"];
  return (<>
    <header className="sec-head white">
      <div className="wrap">
        <div><span className="lab">工具怎么选 · 核实于{VERIFIED}</span><h1>先选对工具，<br />再谈做东西。</h1></div>
        <p>我们最推荐Claude Code和Codex；在国内，最推荐DeepSeek Harness，也可以用CC Switch把Claude Code、Codex接到国产模型上。价格和额度每隔几周就变，每一条都写了核实日期并链到官方价格页，下单前以官网为准。</p>
      </div>
    </header>
    <div className="wrap">
      <section className="cat-block" id="picks">
        <h2>本站推荐<small>按推荐程度排，点名字看详细对比</small></h2>
        <div className="rank-grid">
          {ranks.map((r) => (
            <div key={r} className={`rank-col rank-${r}`}>
              <h3>{RANK_LABEL[r]}</h3>
              {TOOLS.filter((t) => t.rank === r).map((t) => (
                <a key={t.id} href={`#${t.id}`} className="rank-item"><b>{t.name}</b><span>{t.pick}</span></a>
              ))}
            </div>
          ))}
        </div>
      </section>
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
        <p>Anthropic和OpenAI的官方账号都不支持中国大陆，名单外使用可能被封号，本站不教任何绕过地区限制的办法。Claude Code、Codex这两个软件可以接别家模型：DeepSeek等国内厂商官方提供了接入文档，用CC Switch把模型切到它们的官方API，人民币按量付费。但要如实说：Anthropic把Claude Code列为仅在支持地区提供的应用，这种用法官方没有表态，本站不替你判断是否合规；Codex命令行是开源软件，限制少一些。最稳妥的是直接用国内最推荐的DeepSeek Harness。</p>
        <p>只推荐接各模型的<b>官方</b>API。网上卖「Claude/GPT额度」的中转站、拼车共享账号，来路不明、可能违反厂商条款、Key还可能泄露，本站一律不推荐。</p>
      </div>
    </div>
  </>);
}
