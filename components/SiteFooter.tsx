import Link from "next/link";
import { NAV } from "@/lib/site";

export default function SiteFooter() {
  return (
    <footer className="site-foot">
      <div className="wrap">
        <div className="f-top">
          <div>
            <Link className="logo" href="/">Coder<b>With</b>AI</Link>
            <p>你负责想清楚，AI负责写出来。一个免费的AI编程入门站。</p>
          </div>
          <nav className="f-links" aria-label="页脚导航">
            {NAV.map((n) => <Link key={n.href} href={n.href}>{n.label}</Link>)}
            <Link href="/about">关于本站</Link>
            <a href="https://www.huasheng.ai" target="_blank" rel="noopener">花叔的橙皮书</a>
            <Link href="/privacy">隐私政策</Link>
          </nav>
        </div>
        <div className="f-end">
          <span>© 2024–2026 CoderWithAI · coderwithai.top</span>
          <span>工具价格与功能变化很快，页面上都写了核实日期，以官方为准</span>
        </div>
      </div>
    </footer>
  );
}
