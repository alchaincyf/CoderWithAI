// 暗色终端卡 = 「机器的话」。内容区（markdown 渲染）和 React 页面共用同一份输出，保证服务端与浏览器一致。
import { markHtml } from "./pen";

export const esc = (s: string) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;").replace(/"/g, "&quot;");

export const COPY_SVG = '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><rect x="5.5" y="5.5" width="8" height="8" rx="2"/><path d="M10.5 5.5V4a1.5 1.5 0 0 0-1.5-1.5H4A1.5 1.5 0 0 0 2.5 4v5A1.5 1.5 0 0 0 4 10.5h1.5"/></svg>';
export const OK_SVG = '<svg viewBox="0 0 16 16" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round" aria-hidden="true"><path d="M3 8.5l3.2 3L13 4.5"/></svg>';

// 提示词里的 `行内命令` 变成 <code>
export const inlineCode = (s: string) => esc(s).replace(/`([^`]+)`/g, "<code>$1</code>");

function highlightShell(line: string) {
  if (/^\s*#/.test(line)) return `<span class="c">${esc(line)}</span>`;
  const parts = line.split(/(\s+)/);
  let first = true;
  return parts.map((p) => {
    if (!p.trim()) return p;
    const e = esc(p);
    if (first) { first = false; return `<span class="k">${e}</span>`; }
    if (/^-{1,2}[\w-]/.test(p)) return `<span class="f">${e}</span>`;
    if (/^["'].*["']?$/.test(p) || /^["']/.test(p) || /["']$/.test(p)) return `<span class="s">${e}</span>`;
    if (/^[~./$]/.test(p) || /\//.test(p)) return `<span class="s">${e}</span>`;
    return e;
  }).join("");
}

export interface TermOpts {
  kind: "prompt" | "sh" | "err";
  text: string;
  title: string;
  tag?: string;
  copy?: string;           // 复制按钮上的字，默认「复制」
  circle?: string;         // 报错卡：圈出这一句（出现在哪行就圈哪行）
  shell?: boolean;         // sh：给命令行加 $
  analytics?: "prompt" | "code";
}

export function termHtml(o: TermOpts) {
  const lines = o.text.replace(/\n+$/, "").split("\n");
  let circled = false, cont = false;
  const rows = lines.map((raw, i) => {
    let html: string;
    if (o.kind === "prompt") html = inlineCode(raw);
    else if (o.kind === "err") {
      html = esc(raw);
      if (o.circle && !circled && raw.includes(o.circle)) {
        circled = true;
        const at = raw.indexOf(o.circle);
        html = esc(raw.slice(0, at)) + markHtml("o", o.circle) + esc(raw.slice(at + o.circle.length));
      }
      html = `<span class="e">${html}</span>`;
    } else if (o.shell) {
      const isCmd = raw.trim() !== "" && !/^\s*#/.test(raw) && !cont && !/^\s/.test(raw);
      html = (isCmd ? '<span class="p">$ </span>' : "") + highlightShell(raw);
      cont = /\\\s*$/.test(raw);
    } else html = esc(raw);
    return `<div class="ln"><i>${o.kind === "sh" ? "" : i + 1}</i><span>${html || "&#8203;"}</span></div>`;
  }).join("");
  const tag = o.tag ? `<span class="tag${o.kind === "err" ? " err" : ""}">${esc(o.tag)}</span>` : "";
  const kind = o.analytics ?? "code";
  return `<figure class="term ${o.kind}" data-copy data-kind="${kind}" data-text="${esc(o.text.replace(/\n+$/, ""))}"><div class="term-h"><b>${esc(o.title)}</b>${tag}<span class="sp"></span><button type="button" class="tbtn copy-btn" aria-label="${o.kind === "prompt" ? "复制提示词" : "复制"}">${COPY_SVG}<span>${o.copy ?? "复制"}</span></button></div><div class="term-b">${rows}</div></figure>`;
}
