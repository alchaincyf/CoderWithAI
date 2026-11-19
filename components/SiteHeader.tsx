"use client";
import Link from "next/link";
import { usePathname } from "next/navigation";
import { useEffect, useState } from "react";
import { NAV } from "@/lib/site";
import SearchPanel from "./SearchPanel";

export default function SiteHeader() {
  const path = usePathname();
  const [open, setOpen] = useState(false);
  const [menu, setMenu] = useState(false);

  useEffect(() => {
    const onKey = (e: KeyboardEvent) => {
      const typing = /INPUT|TEXTAREA|SELECT/.test((e.target as HTMLElement)?.tagName ?? "");
      if ((e.key === "k" && (e.metaKey || e.ctrlKey)) || (e.key === "/" && !typing)) { e.preventDefault(); setOpen(true); }
    };
    window.addEventListener("keydown", onKey);
    return () => window.removeEventListener("keydown", onKey);
  }, []);
  useEffect(() => { setMenu(false); }, [path]);

  return (
    <header className="site-head">
      <div className="wrap">
        <Link className="logo" href="/" aria-label="CoderWithAI 首页">Coder<b>With</b>AI</Link>
        <nav className={`main-nav${menu ? " open" : ""}`} aria-label="主导航">
          {NAV.map((n) => (
            <Link key={n.href} href={n.href} aria-current={path?.startsWith(n.href) ? "page" : undefined}>{n.label}</Link>
          ))}
        </nav>
        <button className="search-btn" type="button" onClick={() => setOpen(true)} aria-label="搜索或粘贴报错">
          <svg width="18" height="18" viewBox="0 0 24 24" fill="none" stroke="currentColor" strokeWidth="3" aria-hidden="true"><circle cx="10.5" cy="10.5" r="6.5"/><path d="M15.5 15.5 21 21"/></svg>
          <span>搜索，或粘贴报错</span><kbd>⌘K</kbd>
        </button>
        <button className="menu-btn" type="button" aria-expanded={menu} onClick={() => setMenu(!menu)}>{menu ? "收起" : "目录"}</button>
      </div>
      {open && <SearchPanel onClose={() => setOpen(false)} />}
    </header>
  );
}
