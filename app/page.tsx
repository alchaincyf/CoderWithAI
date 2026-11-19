import Link from "next/link";
import HeroDemo from "@/components/HeroDemo";
import RescueDesk from "@/components/RescueDesk";
import LevelFlags from "@/components/LevelFlags";
import { LEVELS } from "@/lib/levels";
import { getDocs, hrefOf } from "@/lib/content";
import { SITE } from "@/lib/site";
import { TOOLS } from "@/lib/tools";

export default function Home() {
  const recipes = getDocs("recipes");
  const fixes = getDocs("fix");
  const words = getDocs("words");
  const pick = <T,>(arr: T[], n: number) => arr.slice(0, n);
  const featuredWords = ["terminal", "api-key", "env-var", "deploy", "git", "mcp"].map((s) => words.find((w) => w.slug === s)).filter(Boolean) as typeof words;
  const featuredFixes = ["command-not-found", "npm-network-timeout", "vercel-build-failed", "undo-ai-changes", "claude-usage-limit"].map((s) => fixes.find((w) => w.slug === s)).filter(Boolean) as typeof fixes;

  const jsonLd = {
    "@context": "https://schema.org",
    "@type": "WebSite",
    name: SITE.name,
    url: SITE.url,
    description: SITE.description,
    inLanguage: "zh-CN",
  };

  return (
    <>
      <script type="application/ld+json" dangerouslySetInnerHTML={{ __html: JSON.stringify(jsonLd) }} />
      <section className="cover">
        <div className="wrap">
          <div className="c-text">
            <span className="kicker lab">AI编程时代的菜鸟教程 · 全部免费</span>
            <h1>不会写代码，<br />也能把想法<br /><em>做出来。</em></h1>
            <p className="sub">从一句话，到一个能发给朋友的网址。CoderWithAI用八关路线、可以直接复制的提示词和报错急救，带你用AI做出第一个真东西。</p>
          </div>
          <HeroDemo />
          <div className="ctas">
            <Link className="btn orange" href="/start/what-is-ai-coding">从第0关开始<small>不装任何东西，先读10分钟</small></Link>
            <Link className="btn" href="/fix">卡住了？粘贴报错<small>红字原样贴进来</small></Link>
          </div>
        </div>
      </section>

      <section className="plan" aria-labelledby="plan-h">
        <div className="plan-l">
          <div className="plan-l-in">
            <span className="lab">闯关路线</span>
            <h2 id="plan-h">八<br />关</h2>
            <p>从完全不懂，一级一级走到有人付费。每一关只解决一件事，每一关都有可以直接复制给AI的提示词。</p>
          </div>
          <span className="dot" aria-hidden="true" />
        </div>
        <div>
          <ol className="steps">
            {LEVELS.map((l) => (
              <li key={l.slug} style={{ ["--i" as string]: l.n }}>
                <Link href={`/start/${l.slug}`}>
                  <span className="n">{l.n}</span>
                  <span className="t">{l.title}<small>{l.sub}</small></span>
                  <LevelFlags slug={l.slug} n={l.n} />
                </Link>
              </li>
            ))}
          </ol>
          <div className="plan-foot">
            <Link className="btn sm" href="/start">看完整路线图</Link>
            <span className="note">进度只存在你自己的浏览器里，不用注册。</span>
          </div>
        </div>
      </section>

      <section className="rescue" aria-labelledby="rescue-h">
        <div className="wrap">
          <div className="rescue-head">
            <h2 id="rescue-h">终端里一串红字？<br />先别慌。</h2>
            <p>把报错原样贴进来。先给你一句人话，再给你一段可以直接复制给AI的话。手册里收了{fixes.length}种新手最常见的报错。</p>
          </div>
          <RescueDesk />
        </div>
      </section>

      <section className="six" aria-labelledby="six-h">
        <div className="wrap">
          <div className="six-head">
            <h2 id="six-h">六个板块，<br />按你卡在哪选。</h2>
            <p>第一次来，从闯关路线开始；想做某个具体东西，看项目配方；做到一半卡住，直接去报错急救。</p>
          </div>
          <div className="bento">
            <Link className="cell c1" href="/start"><span className="no">8<small>关</small></span><h3>闯关路线</h3><p>八关，从搞懂AI编程到有人付费，一步接一步。</p><span className="go">进入</span></Link>
            <Link className="cell c2" href="/recipes"><span className="no">{recipes.length}<small>个</small></span><h3>项目配方</h3><p>个人主页、记账工具、小游戏、小程序……每个配方都是一串能照着发给AI的提示词。</p><span className="go">进入</span></Link>
            <Link className="cell c3" href="/fix"><span className="no">{fixes.length}<small>条</small></span><h3>报错急救</h3><p>粘贴红字，看人话解释，拿到修复提示词。</p><span className="go">进入</span></Link>
            <Link className="cell c4" href="/words"><span className="no">{words.length}<small>个</small></span><h3>人话词典</h3><p>API、部署、环境变量……AI嘴里的行话，翻成人话。</p><span className="go">进入</span></Link>
            <Link className="cell c5" href="/tools"><span className="no">{TOOLS.length}<small>款</small></span><h3>工具怎么选</h3><p>Claude Code、Codex、Cursor、Trae，回答三个问题告诉你用哪个。</p><span className="go">进入</span></Link>
            <Link className="cell c6" href="/brief"><span className="no">1<small>页</small></span><h3>需求说明书生成器</h3><p>把脑子里的想法填进几个空，生成一页AI一读就懂的需求说明。</p><span className="go">进入</span></Link>
          </div>
        </div>
      </section>

      <section className="fresh" aria-label="精选内容">
        <div className="wrap">
          <div className="fresh-col">
            <h3>照着做 <Link href="/recipes">全部配方</Link></h3>
            <ul>{pick(recipes, 5).map((r) => <li key={r.slug}><Link href={hrefOf(r)}>{r.title}<small>{r.summary}</small></Link></li>)}</ul>
          </div>
          <div className="fresh-col">
            <h3>新手最常卡 <Link href="/fix">全部急救</Link></h3>
            <ul>{featuredFixes.map((r) => <li key={r.slug}><Link href={hrefOf(r)}>{r.title}<small>{r.summary}</small></Link></li>)}</ul>
          </div>
          <div className="fresh-col">
            <h3>先认识这几个词 <Link href="/words">全部词条</Link></h3>
            <ul>{featuredWords.map((r) => <li key={r.slug}><Link href={hrefOf(r)}>{r.title}<small>{r.summary}</small></Link></li>)}</ul>
          </div>
        </div>
      </section>

      <section className="author" aria-labelledby="author-h">
        <span className="disc" aria-hidden="true" />
        <div className="wrap">
          <h2 id="author-h">本站作者花叔，<br /><mark>从来不会写代码。</mark><br /><span className="nb-lg">他所有的产品，都是AI写的。</span></h2>
          <div>
            <p>2024年底，他用Cursor做出了「小猫补光灯」。这个站的标准只有一条：一个完全不会写代码的人照着做，能做出来。</p>
            <span className="by"><Link href="/about">关于本站和作者</Link></span>
          </div>
        </div>
      </section>
    </>
  );
}
