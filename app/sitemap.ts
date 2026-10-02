import type { MetadataRoute } from "next";
import { getAvailableLanguages, getTutorials, Tutorial } from "@/lib/tutorials";

const baseUrl = "https://www.coderwithai.top";

// 教程是按目录嵌套的：目录本身不是页面，只收叶子上的 .md 页面
function collectPaths(items: Tutorial[], out: string[]) {
  for (const t of items) {
    if (t.items) collectPaths(t.items, out);
    else out.push(t.path);
  }
}

const encodePath = (p: string) => p.split("/").map(encodeURIComponent).join("/");

export default async function sitemap(): Promise<MetadataRoute.Sitemap> {
  const languages = await getAvailableLanguages();
  const entries: MetadataRoute.Sitemap = [{ url: baseUrl }];

  for (const lang of languages) {
    entries.push({ url: `${baseUrl}/${encodeURIComponent(lang)}` });
    const paths: string[] = [];
    collectPaths(await getTutorials(lang), paths);
    for (const p of paths) {
      entries.push({ url: `${baseUrl}/${encodeURIComponent(lang)}/${encodePath(p)}` });
    }
  }

  return entries;
}
