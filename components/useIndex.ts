"use client";
import { useEffect, useState } from "react";
import type { IndexItem } from "@/lib/match";

let cached: Promise<IndexItem[]> | null = null;
export function loadIndex() {
  if (!cached) cached = fetch("/search-index.json").then((r) => r.json()).catch(() => { cached = null; return []; });
  return cached;
}
export function useIndex(enabled = true) {
  const [items, setItems] = useState<IndexItem[] | null>(null);
  useEffect(() => { if (enabled) loadIndex().then(setItems); }, [enabled]);
  return items;
}
