import { getDocs, hrefOf, type Section } from "./content";
import type { IndexItem } from "./match";

function firstPrompt(html: string) {
  const m = html.match(/<figure class="prompt"[\s\S]*?<pre>([\s\S]*?)<\/pre>/);
  if (!m) return undefined;
  return m[1].replace(/&lt;/g, "<").replace(/&gt;/g, ">").replace(/&quot;/g, '"').replace(/&amp;/g, "&");
}

export function buildIndex(): IndexItem[] {
  const items: IndexItem[] = [];
  (["path", "recipes", "fix", "words"] as Section[]).forEach((s) => {
    for (const d of getDocs(s)) {
      const k: string[] = [...(d.data.aka ?? []), ...(d.data.match ?? []), ...(d.data.tags ?? [])].map(String);
      items.push({
        s, h: hrefOf(d), t: d.title, d: d.summary, k,
        c: d.data.category ? String(d.data.category) : undefined,
        p: s === "fix" ? firstPrompt(d.body) : undefined,
      });
    }
  });
  items.push(
    { s: "page", h: "/tools", t: "AI编程工具怎么选", d: "Claude Code、Codex、Cursor、Trae……价格、能不能在国内用、适合谁", k: ["工具", "对比", "价格", "推荐", "claude code", "codex", "cursor", "trae"] },
    { s: "page", h: "/brief", t: "需求说明书生成器", d: "把脑子里的想法，整理成AI一读就懂的一页说明", k: ["需求", "prd", "说明书", "怎么描述需求"] },
  );
  return items;
}
