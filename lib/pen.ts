// 朱笔批注：手抖路径按「文字内容 + 种类」做种子，同一处每次画出来一样，服务端与浏览器一致。
export type PenKind = "o" | "u" | "s";

function seeded(t: string) {
  let h = 7;
  for (const c of t) h = (h * 31 + c.charCodeAt(0)) % 233280;
  return () => (h = (h * 9301 + 49297) % 233280) / 233280;
}
function smooth(p: number[][]) {
  let d = `M${p[0][0].toFixed(1)} ${p[0][1].toFixed(1)}`;
  for (let i = 1; i < p.length - 1; i++) {
    const m = [(p[i][0] + p[i + 1][0]) / 2, (p[i][1] + p[i + 1][1]) / 2];
    d += ` Q${p[i][0].toFixed(1)} ${p[i][1].toFixed(1)} ${m[0].toFixed(1)} ${m[1].toFixed(1)}`;
  }
  const l = p[p.length - 1];
  return d + ` L${l[0].toFixed(1)} ${l[1].toFixed(1)}`;
}

export function penPath(kind: PenKind, text: string): { viewBox: string; d: string } {
  const r = seeded(text + kind);
  if (kind === "o") {
    const p: number[][] = [], n = 26, a0 = -2.75 + (r() - 0.5) * 0.3, turn = Math.PI * 2 * (1.08 + r() * 0.06);
    for (let i = 0; i <= n; i++) {
      const t = i / n, a = a0 + t * turn, j = 1 + (r() - 0.5) * 0.07, drift = t * 3.2;
      p.push([50 + Math.cos(a) * 47.5 * j, 20 + Math.sin(a) * 16.5 * j - drift + (1 - t) * 1.2]);
    }
    return { viewBox: "0 0 100 40", d: smooth(p) };
  }
  if (kind === "u") {
    const p: number[][] = [], n = 7, y0 = 4 + r() * 1.4, y1 = 4.4 + (r() - 0.5) * 2;
    for (let i = 0; i <= n; i++) { const t = i / n; p.push([t * 100, y0 + (y1 - y0) * t + (r() - 0.5) * 2.2]); }
    return { viewBox: "0 0 100 9", d: smooth(p) };
  }
  const p: number[][] = [], n = 6;
  for (let i = 0; i <= n; i++) { const t = i / n; p.push([t * 100, 5 - t * 2.5 + (r() - 0.5) * 1.6]); }
  return { viewBox: "0 0 100 8", d: smooth(p) };
}

const esc = (s: string) => s.replace(/&/g, "&amp;").replace(/</g, "&lt;").replace(/>/g, "&gt;");

// 给内容里的 HTML 字符串用（服务端渲染的 markdown 区域）
export function markHtml(kind: PenKind, text: string, inner?: string) {
  const { viewBox, d } = penPath(kind, text);
  return `<span class="mk ${kind}">${inner ?? esc(text)}<svg viewBox="${viewBox}" preserveAspectRatio="none" aria-hidden="true"><path pathLength="1" d="${d}"/></svg></span>`;
}
