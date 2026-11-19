import fs from "node:fs";
import path from "node:path";

// 花叔的真实原话（逐字，来自即刻/公众号/B站/知识星球），由 content/stories.json 维护
export interface Story {
  target: string;          // level-0 … level-7 / fix-<slug>
  quote: string;
  date: string;
  source: string;          // 即刻 / AI探索站 / 公众号 / B站 / 知识星球
  url?: string | null;     // 公开原帖链接
  title?: string | null;   // 公开发表时的标题
}

let cache: Story[] | null = null;
export function getStories(): Story[] {
  if (cache) return cache;
  const p = path.join(process.cwd(), "content", "stories.json");
  cache = fs.existsSync(p) ? JSON.parse(fs.readFileSync(p, "utf8")) : [];
  return cache!;
}
export function storiesFor(target: string) {
  return getStories().filter((s) => s.target === target);
}
