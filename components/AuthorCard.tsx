import Link from "next/link";
import { AUTHOR, BOOKS, type BookId } from "@/lib/author";

export function Byline({ updated }: { updated: string }) {
  return (
    <p className="byline">
      <Link href={AUTHOR.url} className="by-av" aria-hidden="true" tabIndex={-1}>花</Link>
      <span>作者 <Link href={AUTHOR.url}>花叔</Link> · {AUTHOR.oneLine}<br />最后核对 {updated}</span>
    </p>
  );
}

export function BookCard({ id }: { id: BookId }) {
  const b = BOOKS[id];
  return (
    <a className="card sc book" href={b.href} target="_blank" rel="noopener">
      <span className="cap">想系统学？读花叔的橙皮书</span>
      <b>{b.title}</b>
      <p>{b.sub}。免费在线读。</p>
    </a>
  );
}

export function AuthorBox() {
  return (
    <aside className="card author-box">
      <Link href={AUTHOR.url} className="by-av big" aria-hidden="true" tabIndex={-1}>花</Link>
      <div>
        <b>关于作者</b>
        <p><Link href={AUTHOR.url}>花叔</Link>，{AUTHOR.title}。{AUTHOR.oneLine}2024年底用Cursor做出「小猫补光灯」，后来写了「橙皮书」系列。这个站里的每一页，都按「不会写代码的人照着能做出来」这个标准写。</p>
        <p className="author-links">{AUTHOR.social.map((s) => <a key={s.href} href={s.href} target="_blank" rel="noopener me">{s.name}</a>)}</p>
      </div>
    </aside>
  );
}
