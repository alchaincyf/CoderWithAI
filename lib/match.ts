// 客户端也会用：不能引用 fs。站内搜索与报错匹配共用一套打分。
export interface IndexItem {
  s: "path" | "recipes" | "fix" | "words" | "page";
  h: string;      // 链接
  t: string;      // 标题
  d: string;      // 一句话
  k: string[];    // 别名 / 报错特征串
  c?: string;     // 分类
  p?: string;     // 急救条目的「复制给AI」提示词
}

export const SECTION_LABEL: Record<IndexItem["s"], string> = {
  path: "路线", recipes: "项目", fix: "急救", words: "词典", page: "页面",
};

const norm = (s: string) => s.toLowerCase().replace(/[“”"'`]/g, "").replace(/\s+/g, " ").trim();

function bigrams(s: string) {
  const out = new Set<string>();
  const t = s.replace(/[\s\p{P}]/gu, "");
  for (let i = 0; i < t.length - 1; i++) out.add(t.slice(i, i + 2));
  return out;
}

export function score(item: IndexItem, rawQ: string): number {
  const q = norm(rawQ);
  if (!q) return 0;
  const title = norm(item.t), desc = norm(item.d), keys = item.k.map(norm);
  let sc = 0;
  // 1. 报错特征串：用户贴进来的一大段里包含它
  for (const k of keys) {
    if (k.length >= 3 && q.includes(k)) sc += 30 + Math.min(k.length, 40);
  }
  // 2. 关键词命中
  if (title === q) sc += 120;
  else if (title.includes(q)) sc += 70;
  if (keys.some((k) => k === q)) sc += 80;
  else if (keys.some((k) => k.includes(q) && q.length >= 2)) sc += 40;
  if (desc.includes(q)) sc += 20;
  for (const tok of q.split(" ").filter((x) => x.length >= 2)) {
    if (title.includes(tok)) sc += 14;
    if (keys.some((k) => k.includes(tok))) sc += 10;
    if (desc.includes(tok)) sc += 5;
  }
  // 3. 中文二元组重合（容错：「怎么上线网站」也能找到「部署」相关）
  if (q.length <= 60) {
    const qb = bigrams(q);
    if (qb.size) {
      const hay = bigrams(title + desc + keys.join(""));
      let hit = 0;
      qb.forEach((b) => { if (hay.has(b)) hit++; });
      sc += (hit / qb.size) * 24;
    }
  }
  return sc;
}

export function search(items: IndexItem[], q: string, limit = 12) {
  return items
    .map((it) => ({ it, sc: score(it, q) }))
    .filter((x) => x.sc >= 8)
    .sort((a, b) => b.sc - a.sc)
    .slice(0, limit)
    .map((x) => x.it);
}

export function matchError(items: IndexItem[], text: string) {
  const fixes = items.filter((i) => i.s === "fix");
  return fixes
    .map((it) => ({ it, sc: score(it, text) }))
    .filter((x) => x.sc >= 20)
    .sort((a, b) => b.sc - a.sc)
    .slice(0, 4)
    .map((x) => x.it);
}
