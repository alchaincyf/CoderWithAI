// 作者信息：事实以花叔本人公开资料为准（huasheng.ai）。不写会过期的数字。
export const AUTHOR = {
  name: "花叔",
  alt: "Alchain",
  url: "/author/huashu",
  home: "https://www.huasheng.ai",
  title: "AI Native Coder · 独立开发者 · AI自媒体博主",
  oneLine: "从来不会写代码，所有产品都是AI写的。",
  facts: [
    "从来不会写代码，他做的所有产品，代码都是AI写的。",
    "2024年底用Cursor做出「小猫补光灯」，登上过App Store付费榜第一。",
    "被CCTV《焦点访谈》作为「手搓经济」的代表人物报道。",
    "写了《一本书玩转DeepSeek》和「橙皮书」系列（包括《Claude Code从入门到精通》）。",
    "开源了女娲.skill等一系列Agent技能。",
  ],
  social: [
    { name: "个人网站", href: "https://www.huasheng.ai" },
    { name: "B站", href: "https://space.bilibili.com/14097567" },
    { name: "YouTube", href: "https://www.youtube.com/@Alchain" },
    { name: "小红书", href: "https://www.xiaohongshu.com/user/profile/5abc6f17e8ac2b109179dfdf" },
    { name: "X", href: "https://x.com/AlchainHust" },
    { name: "GitHub", href: "https://github.com/alchaincyf" },
  ],
};

export const BOOKS = {
  "claude-code": { title: "Claude Code从入门到精通", sub: "一周内用AI独立构建一个完整产品", href: "https://www.huasheng.ai/orange-books/claude-code/" },
  codex: { title: "OpenAI Codex从入门到精通", sub: "五种形态的AI编程Agent", href: "https://www.huasheng.ai/orange-books/codex/" },
  cursor: { title: "Cursor从入门到精通", sub: "用AI编程做出你想做的东西", href: "https://www.huasheng.ai/orange-books/cursor/" },
  "agent-skills": { title: "Agent Skills", sub: "让AI记住你的工作方式", href: "https://www.huasheng.ai/orange-books/agent-skills/" },
  "3d-vibe-coding": { title: "3D Vibe Coding手册", sub: "一个不会写代码的人怎么做3D", href: "https://www.huasheng.ai/orange-books/3d-vibe-coding/" },
} as const;
export type BookId = keyof typeof BOOKS;

// 给某一页挑一本最相关的橙皮书
export function bookFor(section: string, slug: string): BookId {
  if (/skill|mcp|agents-md|subagent|plan-mode|agent|context/.test(slug)) return "agent-skills";
  if (/codex/.test(slug)) return "codex";
  if (/cursor|editor/.test(slug)) return "cursor";
  if (/mini-game/.test(slug)) return "3d-vibe-coding";
  return "claude-code";
}

export const AUTHOR_LD = {
  "@type": "Person",
  name: AUTHOR.name,
  alternateName: AUTHOR.alt,
  url: "https://www.coderwithai.top/author/huashu",
  jobTitle: "AI Native Coder / 独立开发者",
  sameAs: AUTHOR.social.map((s) => s.href),
};
