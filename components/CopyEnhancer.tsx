"use client";
import { useEffect } from "react";
import { copyText } from "./copy";
import { track } from "./track";
import { OK_SVG } from "@/lib/term";

// 文章里提示词卡和代码块的「复制」按钮：事件委托，服务端渲染的 HTML 不用改。复制出去的永远是 data-text 里的原文。
export default function CopyEnhancer() {
  useEffect(() => {
    const onClick = (e: MouseEvent) => {
      const btn = (e.target as HTMLElement).closest(".copy-btn") as HTMLButtonElement | null;
      if (!btn) return;
      const fig = btn.closest("[data-copy]") as HTMLElement | null;
      const text = fig?.dataset.text;
      if (!fig || text == null) return;
      const isPrompt = fig.dataset.kind === "prompt";
      track(isPrompt ? "copy_prompt" : "copy_code", { page: location.pathname });
      copyText(text).then(() => {
        const old = btn.dataset.html ?? btn.innerHTML;
        btn.dataset.html = old;
        btn.innerHTML = OK_SVG + `<span>${isPrompt ? "已复制，去粘给AI" : "已复制"}</span>`;
        btn.classList.add("ok");
        setTimeout(() => { btn.innerHTML = old; btn.classList.remove("ok"); }, 2200);
      });
    };
    document.addEventListener("click", onClick);
    return () => document.removeEventListener("click", onClick);
  }, []);
  return null;
}
