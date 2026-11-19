"use client";
import { useDone } from "./progress";

export default function LevelFlags({ slug, n }: { slug: string; n: number }) {
  const done = useDone();
  if (done.includes(slug)) return <span className="flag done">已过关</span>;
  if (n === 3) return <span className="flag">从这里有网址</span>;
  if (n === 7) return <span className="flag">终点</span>;
  return null;
}
