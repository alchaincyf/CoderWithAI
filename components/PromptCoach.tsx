"use client";
import { useEffect, useMemo, useState } from "react";
import { copyText } from "./copy";
import { track } from "./track";
import TermView from "./TermView";
import Mk from "./Mk";

interface Field { key: string; label: string; hint: string; example: string; required: boolean }
export interface Scenario {
  id: string; name: string; when: string;
  techniques: { rule: string; why: string; example: string | null; solid: boolean }[];
  template: { text: string; fields: Field[] };
  pitfalls: { bad: string; fix: string }[];
}
const KEY = "cwa-prompts";

export default function PromptCoach({ data }: { data: Scenario[] }) {
  const [id, setId] = useState(data[0].id);
  const [vals, setVals] = useState<Record<string, Record<string, string>>>({});
  const [copied, setCopied] = useState(false);
  useEffect(() => {
    try { const s = localStorage.getItem(KEY); if (s) setVals(JSON.parse(s)); } catch {}
    const h = location.hash.slice(1); if (data.some((d) => d.id === h)) setId(h);
  }, [data]);
  useEffect(() => { try { localStorage.setItem(KEY, JSON.stringify(vals)); } catch {} setCopied(false); }, [vals, id]);
  const sc = data.find((d) => d.id === id)!;
  const v = vals[id] ?? {};
  const val = (f: Field) => v[f.key] ?? f.example;
  const out = useMemo(() => sc.template.text.replace(/\{([^}]+)\}/g, (_, k) => {
    const f = sc.template.fields.find((x) => x.key === k);
    return f ? (val(f).trim() || `（${f.label}）`) : `{${k}}`;
  }), [sc, v]); // eslint-disable-line react-hooks/exhaustive-deps
  const pick = (nid: string) => { setId(nid); history.replaceState(null, "", `#${nid}`); };
  return (
    <div className="coach">
      <nav className="coach-tabs" aria-label="场景">
        {data.map((d) => <button key={d.id} type="button" className="chip" aria-pressed={d.id === id} onClick={() => pick(d.id)}>{d.name}</button>)}
      </nav>
      <p className="coach-when"><span className="pill">什么时候用</span> {sc.when}</p>
      <div className="coach-grid">
        <section className="card coach-tips">
          <h2>{sc.name}：{sc.techniques.length}条技巧</h2>
          <ol>
            {sc.techniques.map((t) => (
              <li key={t.rule}>
                <b>{t.rule}</b>
                <p>{t.why}</p>
                {t.example && <blockquote><span className="tag">花叔是这么说的</span>「{t.example}」</blockquote>}
              </li>
            ))}
          </ol>
          {sc.pitfalls.length > 0 && (<>
            <h3>常见说法，和更好的说法</h3>
            <ul className="coach-pit">
              {sc.pitfalls.map((p) => <li key={p.bad}><Mk k="s" wrap>{p.bad}</Mk><span>{p.fix}</span></li>)}
            </ul>
          </>)}
        </section>
        <section className="coach-build">
          <div className="brief-out">
            <TermView text={out} title="生成的话" tag="直接发给AI" copied={copied}
              onCopy={() => { track("copy_prompt_coach", { scenario: id }); copyText(out).then(() => setCopied(true)); }}
              extra={<button type="button" className="tbtn" onClick={() => setVals({ ...vals, [id]: {} })}>恢复示例</button>} />
            <p className="cap" style={{ marginTop: 14 }}>你填的内容只存在这台设备的浏览器里，不会上传。</p>
          </div>
          <div className="card brief-form">
            {sc.template.fields.map((f) => (
              <label key={f.key}>{f.label}{f.required ? "" : "（可不填）"}<small>{f.hint}</small>
                <textarea rows={2} value={val(f)} onChange={(e) => setVals({ ...vals, [id]: { ...v, [f.key]: e.target.value } })} />
              </label>
            ))}
          </div>
        </section>
      </div>
    </div>
  );
}
