---
title: MCP
slug: mcp
aka: ["MCP", "Model Context Protocol", "MCP Server", "mcp服务器", "连接器"]
summary: 让AI接上外部工具和数据的标准接口，像给AI插上各种转接头。
category: AI
level: 5
related: [agent, skill, api, api-key, subagent]
updated: 2026-11-18
---

## 一句话
MCP（Model Context Protocol）是一个开源的通用标准，让Claude Code这类AI工具可以连接外部的服务，比如数据库、浏览器、文档、设计稿，然后直接读取和操作。

## 打个比方
像USB接口。不管是U盘还是键盘，只要遵守同一个接口标准，插上就能用。MCP就是AI世界里的这个接口。

不贴切的地方：USB设备插上基本不会「骗」你，MCP服务器是别人写的程序，可能带来风险，所以接入前要确认可信。

## 你会在哪儿碰到它
- AI说「我们给它接一个Supabase的MCP」「装一个浏览器MCP让我能看网页」。
- 官方文档里添加远程服务器的命令形如：

```bash
claude mcp add --transport http <名称> <服务器网址>
```

- 在Claude Code里输入 `/mcp`，可以查看各服务器连接状态、完成登录授权，或临时关掉某个。
- 本地运行的服务器（stdio）会在你的电脑上启动一个程序，所以要格外留意来源。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
我想让你能直接操作<某服务>，听说有官方MCP。
请先去该服务的官方文档确认：有没有官方MCP、怎么安装、需要什么权限、会不会能删除数据。
先告诉我结论和风险，我同意后你再用claude mcp add安装。
如果需要密钥，提醒我只给只读权限的那一个。
```

## 容易踩的坑
- 来路不明的MCP：官方文档明确提醒，接入前要确认信任该服务器，会抓取外部内容的服务器还可能带来提示词注入风险（网页里藏指令诱导AI）。
- 权限给太大：能写能删的凭证，先换成只读的。
- 装太多：每个服务器都占用[上下文窗口](/words/context-window)，不用的用 `/mcp` 关掉。
- 很多服务其实有命令行工具（如 `gh`），直接让AI运行命令，比装MCP更省事。
- 它能做什么取决于对方服务开放了什么，别预期它无所不能。
