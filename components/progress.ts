"use client";
import { useEffect, useState } from "react";

const KEY = "cwa-done-levels";
export function readDone(): string[] {
  try { return JSON.parse(localStorage.getItem(KEY) || "[]"); } catch { return []; }
}
export function writeDone(v: string[]) {
  try { localStorage.setItem(KEY, JSON.stringify(v)); } catch {}
  window.dispatchEvent(new Event("cwa-progress"));
}
export function useDone() {
  const [done, setDone] = useState<string[]>([]);
  useEffect(() => {
    const sync = () => setDone(readDone());
    sync();
    window.addEventListener("cwa-progress", sync);
    window.addEventListener("storage", sync);
    return () => { window.removeEventListener("cwa-progress", sync); window.removeEventListener("storage", sync); };
  }, []);
  return done;
}
