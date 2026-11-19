import type { Metadata } from "next";
import Link from "next/link";
import { AUTHOR, AUTHOR_LD, BOOKS } from "@/lib/author";

export const metadata: Metadata = {
  title: "花叔：不会写代码的AI Native Coder，CoderWithAI作者",
  description: "花叔（Alchain），AI Native Coder、独立开发者。从来不会写代码，所有产品都是AI写的：小猫补光灯、橙皮书系列、女娲.skill。CoderWithAI是他为不会写代码的人做的AI编程入门站。",
  alternates: { canonical: AUTHOR.url },
  openGraph: { type: "profile", title: "花叔：不会写代码的AI Native Coder", url: AUTHOR.url, images: [{ url: "/og.png", width: 1200, height: 630 }] },
};

export default function Page() {
  const ld = { "@context": "https://schema.org", "@type": "ProfilePage", mainEntity: { ...AUTHOR_LD, description: AUTHOR.oneLine, knowsAbout: ["AI编程", "Vibe Coding", "Claude Code", "独立开发", "AI教育"] } };
  return (<>
    <script type="application/ld+json" dangerouslySetInnerHTML={{ __html: JSON.stringify(ld) }} />
    <header className="sec-head lemon author-hero">
      <div className="wrap">
        <div>
          <span className="lab">本站作者</span>
          <h1>花叔<br /><mark>从来不会写代码。</mark></h1>
        </div>
        <div>
          <p>{AUTHOR.title}。他做的所有产品，代码都是AI写的。CoderWithAI是他为和他一样的人做的：有想法、不会写代码，但想亲手把东西做出来。</p>
          <p className="author-links" style={{ marginTop: 18 }}>{AUTHOR.social.map((s) => <a key={s.href} href={s.href} target="_blank" rel="noopener me">{s.name}</a>)}</p>
        </div>
      </div>
    </header>
    <div className="wrap">
      <section className="cat-block">
        <h2>他是谁</h2>
        <ul className="fact-list">{AUTHOR.facts.map((f) => <li key={f}>{f}</li>)}</ul>
      </section>
      <section className="cat-block">
        <h2>橙皮书：想系统学就读这几本<small>免费在线读</small></h2>
        <div className="recipe-grid" style={{ marginTop: 20 }}>
          {Object.entries(BOOKS).map(([id, b], i) => (
            <a key={id} href={b.href} target="_blank" rel="noopener" className={`recipe${i === 0 ? " feature" : ""}`}>
              <div className="meta"><span>橙皮书</span></div>
              <h3>{b.title}</h3><p>{b.sub}</p><span className="go">去读</span>
            </a>
          ))}
        </div>
      </section>
      <section className="cat-block" style={{ marginBottom: 88 }}>
        <h2>为什么做这个站</h2>
        <div className="prose" style={{ maxWidth: 760, marginTop: 14 }}>
          <p>AI已经能把代码写得很好了。挡在普通人前面的，不再是代码，而是那些没人写下来的东西：终端里一串红字是什么意思、网站做好了怎么让别人打开、哪些钱该花、AI把项目改坏了怎么回去。</p>
          <p>花叔自己就是这么一路踩过来的。这个站把这些路写成了八关路线、报错急救和人话词典，标准只有一条：一个完全不会写代码的人照着做，能做出来。</p>
          <p>从<Link href="/start/what-is-ai-coding">第0关</Link>开始，或者直接去<Link href="/fix">报错急救</Link>。</p>
        </div>
      </section>
    </div>
  </>);
}
