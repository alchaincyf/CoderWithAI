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
    { s: "page", h: "/skills", t: "Skills专题：给AI装上技能包", d: "Skill是什么、怎么装、推荐哪些，按场景分类", k: ["skill", "skills", "技能包", "agent skills", "skill推荐", "怎么装skill", "女娲", "huashu-design"] },
    { s: "page", h: "/prompts", t: "提示词技巧：怎么跟AI说话", d: "提需求、修bug、改设计、要流量、加功能，按场景填空生成发给AI的话", k: ["提示词", "prompt", "怎么跟ai说", "怎么提需求", "修bug", "改设计", "seo", "流量"] },
    { s: "page", h: "/brief", t: "需求说明书生成器", d: "把脑子里的想法，整理成AI一读就懂的一页说明", k: ["需求", "prd", "说明书", "怎么描述需求"] },
  );
  return items;
}
