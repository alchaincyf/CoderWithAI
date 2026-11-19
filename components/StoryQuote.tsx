import Link from "next/link";
import type { Story } from "@/lib/stories";

export default function StoryQuote({ story, compact = false }: { story: Story; compact?: boolean }) {
  const src = story.title ? `${story.source}《${story.title}》` : story.source;
  return (
    <figure className={`story${compact ? " compact" : ""}`}>
      <span className="story-k">花叔当时</span>
      <blockquote>{story.quote}</blockquote>
      <figcaption>
        <Link href="/author/huashu">花叔</Link>，{story.date.slice(0, 7).replace("-", "年")}{story.date.length >= 7 ? "月" : ""}，
        {story.url ? <a href={story.url} target="_blank" rel="noopener">{src}</a> : src}
      </figcaption>
    </figure>
  );
}
