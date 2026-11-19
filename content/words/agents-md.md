---
title: AGENTS.md与CLAUDE.md（给AI的项目说明书）
slug: agents-md
aka: ["AGENTS.md", "CLAUDE.md", "项目说明书", "项目规则", "memory", "记忆文件", "/init"]
summary: 放在项目里的一份说明，AI每次开工前都会先读。
category: AI
level: 1
related: [skill, prompt, context-window, agent, plan-mode]
updated: 2026-11-18
---

## 一句话
AGENTS.md和CLAUDE.md都是放在项目里的普通文本文件，写着「这个项目是什么、怎么运行、有什么规矩」。AI每次开新会话前先读它，不用你重复交代。

## 打个比方
像给新来的同事的入职说明：公司做什么、代码怎么跑、哪些地方千万别动。写一次，每个新人（每次新会话）都能用。

不贴切的地方：同事会记住，AI每个会话都是新人，所以必须写下来。而且它是「建议」而不是「铁律」，AI大多会遵守，但不保证。

## 你会在哪儿碰到它
- 项目根目录出现 `CLAUDE.md` 或 `AGENTS.md`。Claude Code读CLAUDE.md；OpenAI Codex读AGENTS.md。
- Claude Code现在也能直接读取AGENTS.md（v2.1.277起），但有条件：只在你的工作目录及其上层没有任何CLAUDE.md时才读；两种都有，只读CLAUDE.md。想兼顾，就在CLAUDE.md里用一行 `@AGENTS.md` 引入它。
- Codex会从项目根往下到当前目录逐层读取，越靠近当前目录的越后读、优先级越高；默认总大小上限32KiB。
- 在Claude Code里运行 `/init` 可以让AI先帮你生成一份初稿。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
请阅读这个项目，为它写一份AGENTS.md，控制在60行以内，只写这些：
1. 项目是做什么的（两句话）；
2. 怎么启动、怎么测试、怎么构建（给出确切命令）；
3. 目录结构里最重要的几处；
4. 必须遵守的规矩（例如：不要动哪些文件、密钥放哪里、提交前要做什么）。
每一条都要是你从代码里确认过的事实，不要编造。
```

## 容易踩的坑
- 一个项目里同时放AGENTS.md和CLAUDE.md，又不做引入：Claude Code只会读CLAUDE.md，AGENTS.md被静默忽略。要么只用一种，要么用 `@AGENTS.md` 串起来。
- 写太长：每个会话都占[上下文窗口](/words/context-window)，规则越多越不被遵守。官方建议保持精简，流程性的内容放进[Skill](/words/skill)。
- 写了过期内容，如不存在的命令：定期让AI核对。
- 把密钥写进去：它通常会随项目上传到GitHub。
