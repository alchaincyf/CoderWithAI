// GA4 事件：复制提示词、过关、报错匹配命中/未命中（未命中的报错特征就是下一批该写的内容）
export function track(name: string, params: Record<string, string | number> = {}) {
  try { (window as unknown as { gtag?: (...a: unknown[]) => void }).gtag?.("event", name, params); } catch {}
}
