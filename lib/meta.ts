import type { Metadata } from "next";
import type { Doc } from "./content";
import { hrefOf } from "./content";

export function docMeta(d: Doc | undefined, titleOverride?: string): Metadata {
  if (!d) return {};
  const url = hrefOf(d);
  const title = titleOverride ?? d.title;
  return {
    title, description: d.summary,
    alternates: { canonical: url },
    openGraph: { title, description: d.summary, url, type: "article", images: [{ url: "/og.png", width: 1200, height: 630 }] },
  };
}
