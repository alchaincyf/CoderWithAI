import type { Metadata } from "next";
import fs from "node:fs";
import path from "node:path";
import { readPage, renderMd } from "@/lib/content";
import { AUTHOR_LD } from "@/lib/author";

interface Skill { id: string; name: string; author: string; author_type: "huashu" | "official" | "community"; category: string; what: string; when: string; repo: string; install: string; install_note?: string; stars_note?: string; license?: string; risk_note?: string }
interface Catalog { verified_on: string; categories: { id: string; name: string; desc: string }[]; skills: Skill[] }

const page = readPage("skills-guide.md");
export const metadata: Metadata = {
  title: "Skills专题：AI的技能包是什么、怎么装、推荐哪些（2026年11月）",
  description: "Skill是给AI的工作手册。讲清楚它是什么、为什么重要、在Claude Code、Codex、DeepSeek Harness里怎么装，并按场景推荐花叔开源的、Anthropic官方的和社区口碑好的Skill。",
  alternates: { canonical: "/skills" },
};

const WHO = { huashu: "花叔", official: "Anthropic官方", community: "社区" } as const;

export default function Page() {
  const cat: Catalog = JSON.parse(fs.readFileSync(path.join(process.cwd(), "content", "skills.json"), "utf8"));
  const md = page.content;
  const iInstall = md.indexOf("## 怎么装"), iRec = md.indexOf("## 现成的推荐");
  const intro = renderMd(md.slice(0, iInstall));
  const how = renderMd(md.slice(iInstall, iRec));
  const recIntro = renderMd(md.slice(iRec).replace("## 现成的推荐", ""));
  const rank = { huashu: 0, official: 1, community: 2 };
  const ld = { "@context": "https://schema.org", "@type": "Article", headline: String(page.data.title), dateModified: "2026-11-18", inLanguage: "zh-CN", author: AUTHOR_LD,
    mentions: cat.skills.map((s) => ({ "@type": "SoftwareSourceCode", name: s.name, codeRepository: s.repo })) };
  return (<>
    <script type="application/ld+json" dangerouslySetInnerHTML={{ __html: JSON.stringify(ld) }} />
    <header className="sec-head ink">
      <div className="wrap">
        <div><span className="lab">Skills专题 · {cat.skills.length}个推荐 · 核实于{cat.verified_on}</span><h1>给AI装上<br />技能包。</h1></div>
        <p>{String(page.data.summary)}</p>
      </div>
    </header>
    <div className="wrap skills-page">
      <div className="prose" dangerouslySetInnerHTML={{ __html: intro.body }} />
      <section id="recommend" className="skills-rec">
        <h2>推荐的Skills</h2>
        <div className="prose" dangerouslySetInnerHTML={{ __html: recIntro.body }} />
        <nav className="skill-cats" aria-label="分类">{cat.categories.map((c) => <a key={c.id} href={`#c-${c.id}`}>{c.name}</a>)}</nav>
        {cat.categories.map((c) => {
          const list = cat.skills.filter((s) => s.category === c.id).sort((a, b) => rank[a.author_type] - rank[b.author_type]);
          if (!list.length) return null;
          return (
            <section key={c.id} id={`c-${c.id}`} className="skill-cat">
              <h3>{c.name}<small>{c.desc}</small></h3>
              <div className="skill-grid">
                {list.map((s) => (
                  <article key={s.id} className={`skill-card by-${s.author_type}`}>
                    <span className="who">{WHO[s.author_type]}</span>
                    <h4><a href={s.repo} target="_blank" rel="noopener">{s.name}</a></h4>
                    <p>{s.what}</p>
                    <p className="when">适合：{s.when}</p>
                    {s.install && <code className="inst">{s.install}</code>}
                    {s.install_note && <p className="inst-note">{s.install_note}</p>}
                    {s.risk_note && <p className="risk">留意：{s.risk_note}</p>}
                    <p className="meta">{[s.license, s.stars_note].filter(Boolean).join(" · ")}</p>
                  </article>
                ))}
              </div>
            </section>
          );
        })}
      </section>
      <div className="prose" dangerouslySetInnerHTML={{ __html: how.body }} />
    </div>
  </>);
}
