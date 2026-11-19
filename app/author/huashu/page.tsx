import type { Metadata } from "next";
import Link from "next/link";
import { AUTHOR, AUTHOR_LD, BOOKS } from "@/lib/author";
import PageHead from "@/components/PageHead";
import Mk from "@/components/Mk";

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
    <PageHead kicker="本站作者" title={<>花叔，从来<Mk k="o">不会写代码</Mk>。</>}>
      {AUTHOR.title}。他做的所有产品，代码都是AI写的。CoderWithAI是他为和他一样的人做的：有想法、不会写代码，但想亲手把东西做出来。
    </PageHead>
    <div className="wrap wide">
      <p className="author-links social">{AUTHOR.social.map((s) => <a key={s.href} className="chip" href={s.href} target="_blank" rel="noopener me">{s.name}</a>)}</p>
      <section className="cat-block">
        <h2>他是谁</h2>
        <div className="facts card pad">{AUTHOR.facts.map((f) => <div className="fact one" key={f}><p>{f}</p></div>)}</div>
      </section>
      <section className="cat-block">
        <h2>橙皮书：想系统学就读这几本<small>免费在线读</small></h2>
        <div className="recipe-grid">
          {Object.entries(BOOKS).map(([id, b]) => (
            <a key={id} href={b.href} target="_blank" rel="noopener" className="card recipe">
              <div className="meta"><span className="pill plain">橙皮书</span></div>
              <h3>{b.title}</h3><p>{b.sub}</p><span className="go">去读</span>
            </a>
          ))}
        </div>
      </section>
      <section className="cat-block" style={{ marginBottom: 88 }}>
        <h2>为什么做这个站</h2>
        <div className="card sheet narrow"><div className="prose">
          <p>AI已经能把代码写得很好了。挡在普通人前面的，不再是代码，而是那些没人写下来的东西：终端里一串红字是什么意思、网站做好了怎么让别人打开、哪些钱该花、AI把项目改坏了怎么回去。</p>
          <p>花叔自己就是这么一路踩过来的。这个站把这些路写成了八关路线、报错急救和人话词典，标准只有一条：一个完全不会写代码的人照着做，能做出来。</p>
          <p>从<Link href="/start/what-is-ai-coding">第0关</Link>开始，或者直接去<Link href="/fix">报错急救</Link>。</p>
        </div></div>
      </section>
    </div>
  </>);
}
