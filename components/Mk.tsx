import { penPath, type PenKind } from "@/lib/pen";

// 朱笔批注：圈 o、划线 u、删除线 s。进入视口时由 MarkReveal 加上 .in，从左到右「写」出来。
export default function Mk({ k, children, text, wrap = false, delay }: { k: PenKind; children: React.ReactNode; text?: string; wrap?: boolean; delay?: number }) {
  const seed = text ?? (typeof children === "string" ? children : "");
  const { viewBox, d } = penPath(k, seed);
  return (
    <span className={`mk ${k}${wrap ? " wrap-ok" : ""}`} data-delay={delay}>
      {children}
      <svg viewBox={viewBox} preserveAspectRatio="none" aria-hidden="true"><path pathLength={1} d={d} /></svg>
    </span>
  );
}
