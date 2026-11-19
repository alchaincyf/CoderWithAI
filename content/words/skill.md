---
title: Skill（技能包）
slug: skill
aka: ["Skill", "技能", "SKILL.md", "Agent Skills", "斜杠命令", "slash command"]
summary: 一份写给AI的专项操作手册，需要时才加载。
category: AI
level: 1
related: [agents-md, agent, mcp, prompt, subagent]
updated: 2026-11-18
---

## 一句话
Skill是一个文件夹，核心是一份 `SKILL.md`，里面写着「遇到某类任务时该怎么做」。AI觉得相关时会自己调用，你也可以用 `/技能名` 手动调用。

## 打个比方
像员工手册里的专项章节：「报销流程」「客户投诉处理」。不会每天都背一遍，需要时翻到那一章照着做。

不贴切的地方：手册放在那儿是死的，Skill里还可以带脚本和参考文件，让AI直接动手。

## 你会在哪儿碰到它
- 在Claude Code里输入 `/` 会出现可用的技能列表。
- 文件位置（以Claude Code官方文档为准）：

```text
~/.claude/skills/<技能名>/SKILL.md      个人，所有项目可用
.claude/skills/<技能名>/SKILL.md        当前项目，可随仓库共享
```

- 文件开头有一段说明（frontmatter），其中 `description` 写它做什么、什么时候用。
- 加载方式：所有技能的名称和描述常驻上下文，正文只有调用时才载入，所以比把一切塞进 [AGENTS.md](/words/agents-md)更省空间。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
我每周都要做一次「把本周数据整理成周报」，步骤基本固定。
请帮我写成一个Skill，放在项目的.claude/skills/weekly-report/下：
1. description写清楚做什么、什么时候该用；
2. 步骤、输出格式、常见错误都写进SKILL.md；
3. 写完教我怎么输入/weekly-report测试一次。
写之前先查Claude Code官方文档确认当前的格式要求。
```

## 容易踩的坑
- description写得含糊：AI就不知道什么时候该用它。写清「做什么＋什么场景触发」。
- 把一次性的事写成Skill：反复出现的流程才值得。
- 装别人的Skill不看内容：它可以带可执行脚本，先读一遍再用。
- 内容越写越长：官方建议 `SKILL.md` 保持精简（500行内），细节拆到旁边的参考文件里。
- 不同AI工具对格式的支持不完全一致，换工具前看对应文档。
