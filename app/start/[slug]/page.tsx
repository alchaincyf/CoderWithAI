import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import ChipLinks from "@/components/ChipLinks";
import DoneBar from "@/components/DoneBar";
import ProgressStrip from "@/components/ProgressStrip";
import SideRescue from "@/components/SideRescue";
import { getDoc, getDocs } from "@/lib/content";
import { docMeta } from "@/lib/meta";
import { LEVELS } from "@/lib/levels";
import { storiesFor } from "@/lib/stories";
import StoryQuote from "@/components/StoryQuote";

export const dynamicParams = false;
export function generateStaticParams() { return getDocs("path").map((d) => ({ slug: d.slug })); }
export async function generateMetadata({ params }: { params: Promise<{ slug: string }> }) {
  return docMeta(getDoc("path", (await params).slug));
}

export default async function Page({ params }: { params: Promise<{ slug: string }> }) {
  const { slug } = await params;
  const doc = getDoc("path", slug);
  if (!doc) notFound();
  const i = LEVELS.findIndex((l) => l.slug === slug);
  const prev = LEVELS[i - 1], next = LEVELS[i + 1];
  const stories = storiesFor(`level-${i}`);
  return (
    <DocPage
      doc={doc} color="blue"
      crumbs={[{ href: "/", label: "首页" }, { href: "/start", label: "闯关路线" }]}
      facts={[
        { k: "用时", v: String(doc.data.time ?? "") },
        { k: "花费", v: String(doc.data.cost ?? "") },
      ]}
      before={<ProgressStrip here={slug} />}
      lead={stories[0] ? <StoryQuote story={stories[0]} /> : null}
      after={<>
        {doc.data.goal && <div className="callout callout-why" style={{ marginTop: 40 }}><b>过关标准</b><p>{doc.data.goal}</p></div>}
        <DoneBar slug={slug} />
        <ChipLinks title="这一关会遇到的词" section="words" slugs={doc.data.words} />
        <ChipLinks title="这一关最常见的报错" section="fix" slugs={doc.data.fixes} />
        <nav className="pager" aria-label="上一关与下一关">
          {prev ? <Link href={`/start/${prev.slug}`}><small>上一关</small><b>{prev.n} · {prev.title}</b></Link> : <Link href="/start"><small>返回</small><b>路线总览</b></Link>}
          {next ? <Link href={`/start/${next.slug}`}><small>下一关</small><b>{next.n} · {next.title}</b></Link> : <Link href="/recipes"><small>毕业了</small><b>去挑一个配方做</b></Link>}
        </nav>
      </>}
      side={<>
        {doc.data.goal && <div className="side-card"><h3>过关标准</h3><p>{doc.data.goal}</p></div>}
        {stories[1] && <StoryQuote story={stories[1]} compact />}
        <SideRescue />
      </>}
    />
  );
}
