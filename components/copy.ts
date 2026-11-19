export async function copyText(text: string) {
  try { await navigator.clipboard.writeText(text); return true; } catch {
    const t = document.createElement("textarea");
    t.value = text; t.style.position = "fixed"; t.style.opacity = "0";
    document.body.appendChild(t); t.select();
    try { document.execCommand("copy"); } catch {}
    t.remove(); return true;
  }
}
