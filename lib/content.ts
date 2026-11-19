import fs from "node:fs";
import path from "node:path";
import matter from "gray-matter";
import { Marked, type Tokens } from "marked";
import { termHtml, esc as escapeHtml } from "./term";

export type Section = "path" | "recipes" | "fix" | "words";

export interface Doc {
  section: Section;
  slug: string;
  title: string;
  summary: string;
  updated: string;
  body: string; // 渲染好的 HTML
  data: Record<string, any>;
  headings: { id: string; text: string }[];
  errorText?: string; // 报错急救：开头的 ```error 原文，由页面头部的终端卡展示
}

const ROOT = path.join(process.cwd(), "content");

function slugifyHeading(text: string, used: Map<string, number>) {
  const base = text.replace(/<[^>]+>/g, "").trim().toLowerCase()
    .replace(/[\s·、，,。.:：？?！!（）()「」"'`]+/g, "-").replace(/^-+|-+$/g, "") || "h";
  const n = used.get(base) ?? 0;
  used.set(base, n + 1);
  return n ? `${base}-${n}` : base;
}

function makeMarked(headings: { id: string; text: string }[], ctx: { hoistError?: boolean; error?: string } = {}) {
  const used = new Map<string, number>();
  const m = new Marked({ gfm: true, breaks: false });
  m.use({
    renderer: {
      // ```prompt 代码块 = 「复制给AI」的提示词卡；```bash 等 = 带复制按钮的代码
      code(token: Tokens.Code) {
        const lang = (token.lang || "").trim();
        const text = token.text;
        if (lang.startsWith("prompt")) {
          const label = lang.slice(6).trim() || "复制给AI";
          const rest = label.replace(/^复制给\s*/, "");
          const tag = label === "复制给AI" ? undefined : (rest && rest !== label ? rest : label);
          return termHtml({ kind: "prompt", text, title: "复制给AI", tag, copy: "复制", analytics: "prompt" });
        }
        if (lang === "error") {
          if (ctx.hoistError && ctx.error === undefined) { ctx.error = text; return ""; }
          return termHtml({ kind: "err", text, title: "你看到的报错", tag: "报错原文" });
        }
        const shell = /^(bash|sh|zsh|shell|console)$/.test(lang);
        return termHtml({ kind: "sh", text, title: shell ? "终端" : (lang || "文本"), shell, analytics: "code" });
      },
      heading(token: Tokens.Heading) {
        const inner = this.parser.parseInline(token.tokens);
        if (token.depth === 2) {
          const id = slugifyHeading(token.text, used);
          headings.push({ id, text: token.text.replace(/[*_`]/g, "") });
          return `<h2 id="${id}">${inner}</h2>`;
        }
        if (token.depth === 3) {
          const step = token.text.match(/^(第\s*\d+\s*步)\s*[：:·]?\s*(.*)$/);
          if (step) return `<h3><small>${escapeHtml(step[1].replace(/\s+/g, ""))}</small><span>${new Marked().parseInline(step[2]) as string}</span></h3>`;
        }
        return `<h${token.depth}>${inner}</h${token.depth}>`;
      },
      // > [!tip] / [!warn] / [!why] 提示框
      blockquote(token: Tokens.Blockquote) {
        const raw = token.text.trim();
        const mm = raw.match(/^\[!(tip|warn|why|note)\]\s*/i);
        const kind = mm ? mm[1].toLowerCase() : "note";
        const label = { tip: "小窍门", warn: "注意", why: "为什么", note: "说明" }[kind];
        const cls = { tip: "", warn: " warn", why: " why", note: " why" }[kind];
        const body = mm ? raw.slice(mm[0].length) : raw;
        return `<aside class="box${cls}"><b>${label}</b><div>${this.parser.parse(new Marked().lexer(body))}</div></aside>`;
      },
      link(token: Tokens.Link) {
        const text = this.parser.parseInline(token.tokens);
        const ext = /^https?:\/\//.test(token.href) && !token.href.includes("coderwithai.top");
        return `<a href="${escapeHtml(token.href)}"${ext ? ' target="_blank" rel="noopener"' : ""}>${text}</a>`;
      },
    },
  });
  return m;
}

// 中文紧贴的 **加粗**（如「**结论。**后面的字」）不满足 CommonMark 的定界规则，会原样显示星号。
// 在代码块和行内代码之外，先把 **…** 换成 <strong>。
function cjkBold(md: string) {
  return md.split(/(^```[\s\S]*?^```)/m).map((part, i) => {
    if (i % 2 === 1) return part;
    return part.split(/(`[^`\n]*`)/).map((seg, j) => (j % 2 === 1 ? seg : seg.replace(/\*\*([^*\n]+?)\*\*/g, "<strong>$1</strong>"))).join("");
  }).join("");
}

const cache = new Map<Section, Doc[]>();

export function getDocs(section: Section): Doc[] {
  if (cache.has(section)) return cache.get(section)!;
  const dir = path.join(ROOT, section);
  if (!fs.existsSync(dir)) return [];
  const docs = fs.readdirSync(dir)
    .filter((f) => f.endsWith(".md") && !f.startsWith("_"))
    .map((f) => {
      const raw = fs.readFileSync(path.join(dir, f), "utf8");
      const { data, content } = matter(raw);
      const headings: { id: string; text: string }[] = [];
      const ctx = { hoistError: section === "fix" && /^\s*```error/.test(content) } as { hoistError?: boolean; error?: string };
      const body = makeMarked(headings, ctx).parse(cjkBold(content)) as string;
      const slug = (data.slug as string) || f.replace(/\.md$/, "").replace(/^\d+-/, "");
      return {
        section, slug,
        title: String(data.title ?? slug),
        summary: String(data.summary ?? ""),
        updated: data.updated instanceof Date ? data.updated.toISOString().slice(0, 10) : String(data.updated ?? "2026-11-18"),
        body, data, headings, errorText: ctx.error,
      } satisfies Doc;
    })
    .sort((a, b) => (a.data.order ?? 999) - (b.data.order ?? 999) || a.title.localeCompare(b.title, "zh"));
  cache.set(section, docs);
  return docs;
}

export function getDoc(section: Section, slug: string) {
  return getDocs(section).find((d) => d.slug === slug);
}

export const SECTION_META: Record<Section, { name: string; href: string }> = {
  path: { name: "闯关路线", href: "/start" },
  recipes: { name: "实战项目", href: "/projects" },
  fix: { name: "报错急救", href: "/fix" },
  words: { name: "人话词典", href: "/words" },
};

export function hrefOf(d: Pick<Doc, "section" | "slug">) {
  return `${SECTION_META[d.section].href}/${d.slug}`;
}

// 单页 markdown（如 skills-guide.md），用同一套渲染规则
export function renderMd(md: string) {
  const headings: { id: string; text: string }[] = [];
  const body = makeMarked(headings).parse(cjkBold(md)) as string;
  return { body, headings };
}
export function readPage(file: string) {
  const { data, content } = matter(fs.readFileSync(path.join(ROOT, file), "utf8"));
  return { data, content };
}
