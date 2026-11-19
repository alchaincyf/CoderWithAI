"use client";
import { useEffect } from "react";
import { copyText } from "./copy";
import { track } from "./track";

// 文章里 ```prompt 和代码块的「复制」按钮：用事件委托，服务端渲染的 HTML 不用改
export default function CopyEnhancer() {
  useEffect(() => {
    const onClick = (e: MouseEvent) => {
      const btn = (e.target as HTMLElement).closest(".copy-btn") as HTMLButtonElement | null;
      if (!btn) return;
      const fig = btn.closest("[data-copy]");
      const pre = fig?.querySelector("pre");
      if (!pre) return;
      track(fig?.classList.contains("prompt") ? "copy_prompt" : "copy_code", { page: location.pathname });
      copyText(pre.innerText.trimEnd()).then(() => {
        const old = btn.dataset.label || btn.textContent || "复制";
        btn.dataset.label = old;
        btn.textContent = fig?.classList.contains("prompt") ? "已复制，去粘给AI" : "已复制";
        btn.classList.add("done");
        setTimeout(() => { btn.textContent = old; btn.classList.remove("done"); }, 2200);
      });
    };
    document.addEventListener("click", onClick);
    return () => document.removeEventListener("click", onClick);
  }, []);
  return null;
}
