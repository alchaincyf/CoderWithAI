// 庆祝烟花：荧光笔黄 + 蓝墨水，不撞色
const COLS = ["#2547c9", "#ffd84d", "#17181c", "#7f99f2", "#ffe27a", "#c9d4f7"];
export function burst(x: number, y: number, n = 26) {
  const c = document.createElement("div");
  c.className = "burst"; c.style.left = x + "px"; c.style.top = y + "px";
  for (let i = 0; i < n; i++) {
    const s = document.createElement("i"), a = Math.random() * Math.PI * 2, d = 50 + Math.random() * 90, sz = 5 + Math.random() * 5;
    s.style.cssText = `--dx:${Math.cos(a) * d}px;--dy:${Math.sin(a) * d}px;background:${COLS[i % COLS.length]};animation-delay:${Math.random() * 0.08}s;width:${sz}px;height:${sz}px`;
    c.appendChild(s);
  }
  document.body.appendChild(c);
  setTimeout(() => c.remove(), 1200);
}
export function burstAt(el: Element | null, n = 26) {
  if (!el) return;
  const r = el.getBoundingClientRect();
  burst(r.left + r.width / 2, r.top, n);
}
