"use client";
import { useEffect, useRef } from "react";

// AdSense 多重广告（原站的广告单元 4428404678）。只放在文章末尾，不打断阅读。
export default function AdSlot() {
  const done = useRef(false);
  useEffect(() => {
    if (done.current) return;
    done.current = true;
    try { ((window as unknown as { adsbygoogle: unknown[] }).adsbygoogle ||= []).push({}); } catch {}
  }, []);
  return (
    <div className="ad-wrap" aria-label="广告">
      <span className="ad-label">广告</span>
      <ins className="adsbygoogle" style={{ display: "block" }} data-ad-format="autorelaxed"
        data-ad-client="ca-pub-7522094408813551" data-ad-slot="4428404678" />
    </div>
  );
}
