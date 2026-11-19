import { notFound } from "next/navigation";
import Link from "next/link";
import DocPage from "@/components/DocPage";
import ChipLinks from "@/components/ChipLinks";
import DoneBar from "@/components/DoneBar";
import { Mini8 } from "@/components/ProgressStrip";
import { PathHead } from "@/components/Heads";
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
  const fixes = (doc.data.fixes ?? []) as string[];
  return (
    <DocPage
      doc={doc}
      head={<PathHead doc={doc} n={i} />}
      lead={stories[0] ? <StoryQuote story={stories[0]} /> : null}
      after={<DoneBar slug={slug} />}
      cards={
        <div className="after">
          <ChipLinks variant="cards" title="这一关会用到的词" section="words" slugs={doc.data.words} />
          <div className="card sc next">
            {next ? (<>
              <span className="cap">下一关</span>
              <b>第{next.n}关：{next.title}</b>
              <p>{next.sub}。</p>
              <Link className="btn" href={`/start/${next.slug}`}>去第{next.n}关</Link>
            </>) : (<>
              <span className="cap">毕业了</span>
              <b>去挑一个项目做</b>
              <p>八关都过了，找个实战项目，照着做出自己的东西。</p>
              <Link className="btn" href="/projects">看实战项目</Link>
            </>)}
            <p className="prev">{prev ? <Link href={`/start/${prev.slug}`}>← 上一关：{prev.title}</Link> : <Link href="/start">← 路线总览</Link>}</p>
          </div>
        </div>
      }
      side={<>
        <Mini8 here={slug} />
        <ChipLinks variant="stuck" title="这一关最常卡住" note="报错急救" section="fix" slugs={fixes} />
        {stories[1] && <StoryQuote story={stories[1]} compact />}
        <SideRescue />
      </>}
    />
  );
}
