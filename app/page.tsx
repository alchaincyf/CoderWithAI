import Link from "next/link";
import fs from "node:fs";
import path from "node:path";
import HeroDemo from "@/components/HeroDemo";
import RescueDesk from "@/components/RescueDesk";
import RouteMap from "@/components/RouteMap";
import Mk from "@/components/Mk";
import { PromptTeaser, MiniBrief, WordPeek, type Teaser } from "@/components/HomeWidgets";
import { getDocs, hrefOf } from "@/lib/content";
import { SITE } from "@/lib/site";
import { TOOLS } from "@/lib/tools";
import { AUTHOR, BOOKS } from "@/lib/author";
import { getStories } from "@/lib/stories";

const FACT_LABEL = ["2024年底", "媒体", "著作"];
const boldQuote = (s: string) => s.split(/(「[^」]+」)/).map((p, i) => (p.startsWith("「") ? <b key={i}>{p}</b> : p));

export default function Home() {
  const recipes = getDocs("recipes");
  const fixes = getDocs("fix");
  const words = getDocs("words");
  const paths = getDocs("path");
  const times: Record<string, string> = {};
  for (const d of paths) if (d.data.time) times[d.slug] = String(d.data.time);

  const scenarios: { name: string; when: string; techniques: { rule: string }[]; pitfalls: { bad: string; fix: string }[] }[] =
    JSON.parse(fs.readFileSync(path.join(process.cwd(), "content", "prompts.json"), "utf8"));
  const teasers: Teaser[] = scenarios.filter((s) => s.pitfalls.length).slice(0, 6).map((s) => ({
    name: s.name.replace(/（.*?）/g, ""), bad: s.pitfalls[0].bad, fix: s.pitfalls[0].fix, when: s.when, rules: s.techniques.slice(0, 3).map((t) => t.rule),
  }));
  const skillsN = (JSON.parse(fs.readFileSync(path.join(process.cwd(), "content", "skills.json"), "utf8")).skills as unknown[]).length;

  const featuredWords = ["terminal", "api-key", "env-var", "deploy", "git", "mcp"].map((s) => words.find((w) => w.slug === s)).filter(Boolean) as typeof words;
  const projectPick = recipes.slice(0, 5);
  const story = getStories().find((s) => s.quote.startsWith("修bug而已")) ?? getStories()[0];
  const tool = (id: string) => TOOLS.find((t) => t.id === id);

  const jsonLd = { "@context": "https://schema.org", "@type": "WebSite", name: SITE.name, url: SITE.url, description: SITE.description, inLanguage: "zh-CN" };

  return (
    <>
      <script type="application/ld+json" dangerouslySetInnerHTML={{ __html: JSON.stringify(jsonLd) }} />

      <header className="hero">
        <div className="wrap">
          <div className="hero-top">
            <div>
              <span className="pill plain">AI编程时代的菜鸟教程，全部免费</span>
              <h1>不会写代码，<br />也能<Mk k="u" delay={500}>把想法做出来</Mk>。</h1>
              <p className="sub">从一句话，到一个能发给朋友的网址。CoderWithAI用八关路线、可以直接复制的提示词和报错急救，带你用AI做出第一个真东西。</p>
            </div>
            <div className="hero-r">
              <Link className="btn btn-big pri" href="/start/what-is-ai-coding"><b>从第0关开始</b><small>不装任何东西，先读10分钟</small></Link>
              <Link className="btn btn-big" href="/fix"><b>卡住了？粘贴报错</b><small>红字原样贴进来</small></Link>
            </div>
          </div>
          <HeroDemo />
        </div>
      </header>

      <section className="sec" id="route" style={{ paddingTop: 40 }} aria-labelledby="route-h">
        <div className="wrap">
          <div className="sec-h">
            <div>
              <h2 id="route-h">闯关路线：<Mk k="o">八关</Mk></h2>
              <p>从完全不懂，一级一级走到有人付费。每一关只解决一件事，每一关都有可以直接复制给AI的提示词。</p>
            </div>
            <Link className="btn" href="/start">看完整路线图</Link>
          </div>
          <RouteMap times={times} />
        </div>
      </section>

      <section className="sec" id="fix" style={{ paddingTop: 24 }} aria-labelledby="fix-h">
        <div className="wrap">
          <div className="sec-h">
            <div>
              <h2 id="fix-h">终端里一串红字？<Mk k="u">先别慌</Mk>。</h2>
              <p>把报错原样贴进来。先给你一句人话，再给你一段可以直接复制给AI的话。手册里收了{fixes.length}种新手最常见的报错。</p>
            </div>
            <Link className="btn" href="/fix">全部急救手册</Link>
          </div>
          <RescueDesk />
        </div>
      </section>

      <section className="sec" id="sections" style={{ paddingTop: 40 }} aria-labelledby="sec-h">
        <div className="wrap">
          <div className="sec-h">
            <div>
              <h2 id="sec-h">八个板块，<Mk k="u">按你卡在哪</Mk>选。</h2>
              <p>第一次来，从闯关路线开始；想做某个具体东西，看实战项目；做到一半卡住，直接去报错急救；不知道怎么跟AI开口，看提示词技巧。</p>
            </div>
          </div>
          <div className="bento">
            <article className="card bc s7">
              <h3>提示词技巧 <span className="pill new">新</span><span className="n">{scenarios.length}个场景</span></h3>
              <p>按场景教你怎么跟AI说话：提需求、修bug、改设计、要流量、加功能……每个场景都有能直接填空的模板。</p>
              <PromptTeaser items={teasers} />
              <Link className="go" href="/prompts">看全部{scenarios.length}个场景</Link>
            </article>
            <article className="card bc s5 brief">
              <h3>需求说明书生成器 <span className="n">1页</span></h3>
              <p>填两个空，下面实时变出一段AI一读就懂的需求。</p>
              <MiniBrief />
              <Link className="go" href="/brief">填完整的说明书</Link>
            </article>
            <article className="card bc s4">
              <h3>实战项目 <span className="n">{recipes.length}个</span></h3>
              <p>每个项目都是一串能照着发给AI的提示词。</p>
              <ul className="list">
                {projectPick.map((r) => <li key={r.slug}><Link href={hrefOf(r)}>{r.title}</Link></li>)}
              </ul>
              <Link className="go" href="/projects">全部{recipes.length}个项目</Link>
            </article>
            <article className="card bc s4">
              <h3>人话词典 <span className="n">{words.length}个词</span></h3>
              <p>AI嘴里的行话，翻成人话。</p>
              <WordPeek words={featuredWords.map((w) => ({ title: w.title, summary: w.summary, href: hrefOf(w) }))} />
              <Link className="go" href="/words">翻整本词典</Link>
            </article>
            <article className="card bc s4">
              <h3>工具怎么选 <span className="n">{TOOLS.length}款</span></h3>
              <p>回答三个问题，告诉你先试哪个。</p>
              <div className="tools">
                <div className="tool top">{tool("claude-code")?.name} <span className="pill">最推荐</span></div>
                <div className="tool top">{tool("codex")?.name} <span className="pill">最推荐</span></div>
                <div className="tool">{tool("deepseek-harness")?.name} <span className="pill plain">国内最推荐</span></div>
              </div>
              <Link className="go" href="/tools">看怎么选</Link>
            </article>
            <Link className="card bc s4 mini" href="/skills"><h3>Skills <span className="pill new">新</span><span className="n">{skillsN}个</span></h3><p>给AI装上技能包：是什么、怎么装、推荐哪些。</p><span className="go">去看看</span></Link>
            <Link className="card bc s4 mini" href="/start"><h3>闯关路线 <span className="n">8关</span></h3><p>从搞懂AI编程到有人付费。</p><span className="go">回到路线</span></Link>
            <Link className="card bc s4 mini" href="/fix"><h3>报错急救 <span className="n">{fixes.length}条</span></h3><p>粘贴红字，看人话，拿到修复提示词。</p><span className="go">进入急救</span></Link>
          </div>
        </div>
      </section>

      <section className="sec author-sec" id="author" aria-labelledby="author-h">
        <div className="wrap author">
          <div>
            <span className="pill plain">本站作者</span>
            <h2 id="author-h">花叔，从来<Mk k="o">不会写代码</Mk>。</h2>
            <p className="lead">他所有的产品，都是AI写的。所以这个站不教语法，教的是一个不会写代码的人，怎么指挥AI把东西做出来、卡住了怎么办。</p>
            <div className="facts">
              {AUTHOR.facts.slice(1, 4).map((f, i) => (
                <div className="fact" key={f}><time>{FACT_LABEL[i]}</time><p>{boldQuote(i === 0 ? f.replace(/^2024年底/, "") : f)}</p></div>
              ))}
            </div>
          </div>
          <figure className="card q">
            <span className="tag">花叔当时</span>
            <blockquote>{story.quote}</blockquote>
            <figcaption>来自花叔的{story.source}，{story.date}</figcaption>
            <div className="q-links">
              <Link className="btn pri" href="/author/huashu">关于花叔</Link>
              <a className="btn" href={BOOKS["claude-code"].href} target="_blank" rel="noopener">读橙皮书</a>
            </div>
          </figure>
        </div>
      </section>
    </>
  );
}
