"use client";
import { COPY_SVG, OK_SVG } from "@/lib/term";

// 客户端用的提示词卡：和内容区的终端卡同一套样式
export default function TermView({ text, title = "复制给AI", tag, copied, onCopy, extra }: { text: string; title?: string; tag?: string; copied: boolean; onCopy: () => void; extra?: React.ReactNode }) {
  return (
    <div className="term prompt">
      <div className="term-h">
        <b>{title}</b>{tag && <span className="tag">{tag}</span>}<span className="sp" />
        {extra}
        <button type="button" className={`tbtn${copied ? " ok" : ""}`} onClick={onCopy}>
          <span dangerouslySetInnerHTML={{ __html: copied ? OK_SVG : COPY_SVG }} />{copied ? "已复制，去粘给AI" : "复制"}
        </button>
      </div>
      <div className="term-b">
        {text.split("\n").map((l, i) => <div className="ln" key={i}><i>{i + 1}</i><span>{l || "​"}</span></div>)}
      </div>
    </div>
  );
}
