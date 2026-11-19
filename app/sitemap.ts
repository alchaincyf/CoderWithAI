import type { MetadataRoute } from "next";
import { getDocs, hrefOf, type Section } from "@/lib/content";
import { SITE } from "@/lib/site";

export default function sitemap(): MetadataRoute.Sitemap {
  const now = "2026-11-18";
  const fixed = ["", "/start", "/projects", "/fix", "/words", "/tools", "/brief", "/prompts", "/about"].map((p) => ({
    url: SITE.url + p, lastModified: now, changeFrequency: "weekly" as const, priority: p === "" ? 1 : 0.8,
  }));
  const docs = (["path", "recipes", "fix", "words"] as Section[]).flatMap((s) =>
    getDocs(s).map((d) => ({ url: SITE.url + hrefOf(d), lastModified: d.updated, changeFrequency: "monthly" as const, priority: s === "path" ? 0.9 : 0.6 })),
  );
  return [...fixed, ...docs];
}
