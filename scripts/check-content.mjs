// 内容体检：frontmatter 能否解析、必填字段、站内链接是否指向存在的页面。--fix 会给含冒号的标量加引号。
import fs from "node:fs";
import path from "node:path";
import matter from "gray-matter";

const ROOT = path.resolve("content");
const SECTIONS = { path: "/start", recipes: "/projects", fix: "/fix", words: "/words" };
const FIX = process.argv.includes("--fix");
const QUOTE_KEYS = ["title", "summary", "goal", "time", "cost"];
const slugs = {}; const problems = []; const docs = [];

for (const [sec, base] of Object.entries(SECTIONS)) {
  const dir = path.join(ROOT, sec); if (!fs.existsSync(dir)) continue;
  slugs[base] = new Set();
  for (const f of fs.readdirSync(dir).filter((x) => x.endsWith(".md") && !x.startsWith("_"))) {
    const p = path.join(dir, f);
    let raw = fs.readFileSync(p, "utf8");
    if (FIX) {
      const fixed = raw.replace(/^---\n([\s\S]*?)\n---/, (m, fm) => "---\n" + fm.split("\n").map((line) => {
        const mm = line.match(/^(\w+):\s+(.*)$/);
        if (mm && QUOTE_KEYS.includes(mm[1]) && !/^["'\[]/.test(mm[2]) && /[:#]/.test(mm[2])) return `${mm[1]}: ${JSON.stringify(mm[2])}`;
        return line;
      }).join("\n") + "\n---");
      if (fixed !== raw) { fs.writeFileSync(p, fixed); raw = fixed; console.log("fixed", sec + "/" + f); }
    }
    try {
      const { data, content } = matter(raw);
      const slug = data.slug || f.replace(/\.md$/, "").replace(/^\d+-/, "");
      slugs[base].add(slug);
      docs.push({ sec, f, data, content });
      for (const k of ["title", "summary"]) if (!data[k]) problems.push(`${sec}/${f}: 缺 ${k}`);
    } catch (e) { problems.push(`${sec}/${f}: frontmatter 解析失败 ${e.reason || e.message}`); }
  }
}
for (const d of docs) {
  for (const m of d.content.matchAll(/\]\((\/(start|projects|recipes|fix|words)\/([a-z0-9-]+))[)#]/g)) {
    if (!slugs["/" + m[2]]?.has(m[3])) problems.push(`${d.sec}/${d.f}: 死链 ${m[1]}`);
  }
  for (const [k, base] of [["related", "/words"], ["words", "/words"], ["fixes", "/fix"]]) {
    for (const s of d.data[k] ?? []) if (!slugs[base]?.has(s)) problems.push(`${d.sec}/${d.f}: ${k} 指向不存在的 ${s}`);
  }
}
console.log(Object.entries(slugs).map(([k, v]) => `${k}:${v.size}`).join("  "));
console.log(problems.length ? problems.join("\n") : "没有问题");
