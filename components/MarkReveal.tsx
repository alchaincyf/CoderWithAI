"use client";
import { useEffect } from "react";

// 全站一个观察器：.mk 进入视口时加 .in 触发书写动画；后来才出现的 .mk（交互里换出来的）也会被接住。
export default function MarkReveal() {
  useEffect(() => {
    const reduce = window.matchMedia("(prefers-reduced-motion: reduce)").matches;
    const io = !reduce && "IntersectionObserver" in window
      ? new IntersectionObserver((es) => es.forEach((e) => {
          if (!e.isIntersecting) return;
          const t = e.target as HTMLElement;
          setTimeout(() => t.classList.add("in"), Number(t.dataset.delay) || 0);
          io!.unobserve(t);
        }), { threshold: 0.6 })
      : null;
    let n = 0;
    const scan = () => {
      document.querySelectorAll<HTMLElement>(".mk:not(.in):not([data-w])").forEach((el) => {
        el.dataset.w = "1";
        if (!el.dataset.delay) el.dataset.delay = String((n++ % 4) * 220);
        if (io) io.observe(el); else el.classList.add("in");
      });
    };
    scan();
    let raf = 0;
    const mo = new MutationObserver(() => { cancelAnimationFrame(raf); raf = requestAnimationFrame(scan); });
    mo.observe(document.body, { childList: true, subtree: true });
    return () => { mo.disconnect(); io?.disconnect(); cancelAnimationFrame(raf); };
  }, []);
  return null;
}
