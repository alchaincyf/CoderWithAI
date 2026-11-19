"use client";
import Link from "next/link";
import { usePathname } from "next/navigation";
import { useEffect, useState } from "react";
import { NAV } from "@/lib/site";
import SearchPanel from "./SearchPanel";
import Brand from "./Brand";

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
    <header className="nav">
      <div className="nav-in">
        <Link className="brand" href="/" aria-label="CoderWithAI 首页"><Brand /></Link>
        <nav className="links" aria-label="主导航">
          {NAV.map((n) => (
            <Link key={n.href} href={n.href} aria-current={path?.startsWith(n.href) ? "page" : undefined}>{n.label}</Link>
          ))}
        </nav>
        <button className="search" type="button" onClick={() => setOpen(true)} aria-label="搜索或粘贴报错">
          <svg viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.6" strokeLinecap="round" aria-hidden="true"><circle cx="7" cy="7" r="4.5" /><path d="M10.5 10.5L14 14" /></svg>
          <span>搜索，或粘贴报错</span><kbd>⌘K</kbd>
        </button>
        <button className="icon-search" type="button" onClick={() => setOpen(true)} aria-label="搜索或粘贴报错">
          <svg viewBox="0 0 16 16" fill="none" stroke="currentColor" strokeWidth="1.6" strokeLinecap="round" aria-hidden="true"><circle cx="7" cy="7" r="4.5" /><path d="M10.5 10.5L14 14" /></svg>
        </button>
        <button className="menu-btn" type="button" aria-expanded={menu} aria-controls="drawer" onClick={() => setMenu(!menu)}>{menu ? "关闭" : "菜单"}</button>
      </div>
      <div className={`drawer${menu ? " open" : ""}`} id="drawer">
        {NAV.map((n) => <Link key={n.href} href={n.href} aria-current={path?.startsWith(n.href) ? "page" : undefined}>{n.label}</Link>)}
      </div>
      {open && <SearchPanel onClose={() => setOpen(false)} />}
    </header>
  );
}
