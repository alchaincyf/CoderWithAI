---
title: 对话太长AI开始忘事（上下文满了、自动压缩compact）怎么办
slug: context-full
summary: 对话越长AI越容易丢细节。用/compact压缩、/clear重开，重要规则写进AGENTS.md别靠聊天记忆。
category: AI工具
match: ["Context limit reached", "Prompt is too long", "/compact or /clear to continue", "auto-compact", "compaction", "Context exceeds", "context window"]
platform: [all]
updated: 2026-11-18
---

```error
Context limit reached · /compact or /clear to continue
```

压缩失败时：

```error
Prompt is too long · automatic compaction failed: <具体原因>
```

## 人话：这是怎么回事

AI一次能「记住」的内容有上限，叫 [上下文窗口](/words/context-window)。你们的聊天记录、它读过的文件、命令输出，全都占位置。对话越长，窗口越满。

快满时，Claude Code会**自动压缩**（auto-compact）：把早期的对话总结成摘要，腾出空间。官方说明，这个功能默认开启。压缩后细节会变少，所以你会觉得「它怎么忘了我们刚才定的规矩」。这是正常的取舍，不是故障。

## 最快的解法：把这段发给AI

压缩前后，让AI把关键信息固化下来：

```prompt 复制给 Claude Code
这个对话已经很长了，我准备压缩/开新对话。请先做交接：
1. 总结当前任务目标、已完成的部分、还没完成的部分、遇到过并解决的坑。
2. 列出我们达成共识的重要决定和约定（技术选型、命名规则、不能碰的文件）。
3. 把这些写进项目根目录的 AGENTS.md（已有就追加到合适位置，不要覆盖原内容），内容要精炼，只写以后每次都需要知道的东西。
4. 写好后告诉我，我再执行 /clear。
```

## 想自己动手的话

三个常用命令（来自Claude Code官方文档）：

```bash
/compact
```

手动压缩，可以带上侧重点，例如 `/compact 重点保留接口设计和已确定的数据结构`，告诉它摘要时保留什么。

```bash
/clear
```

清空对话，从零开始。适合换到不相关的任务。官方也建议任务之间要清空，旧内容会在每次请求里白白占用额度。

```bash
/context
```

查看上下文被什么占用了（对话、MCP工具等）。

另外，`/rewind` 的菜单里有 **Summarize from here**（从这里往后压缩）和 **Summarize up to here**（压缩这之前的内容）两个选项，可以只压缩一部分，保留最近的细节不动。

其他工具（Codex、Cursor等）也有类似的压缩或新开对话功能，命令名称各不相同，以各自官方文档为准。

> [!warn] 压缩是有损的。决定、约定、注意事项这类「必须记住」的内容，不要寄希望于聊天记忆，写进 [AGENTS.md](/words/agents-md) 或项目文档，AI每次开场都会读到。

## 下次怎么避免

一个对话只做一件事，做完就 `/clear`。大功能拆成几步，每步一个新对话。长期规则写进AGENTS.md。让AI做大范围查找时，用子代理，只把结论带回主对话。

## 还不行？

压缩失败（compaction failed）时，先按提示里的原因处理，因为 `/compact` 会遇到同样的错误。用量也在飞快消耗的话，看 [claude-usage-limit](/fix/claude-usage-limit)。AI反复改不好同一个问题，看 [ai-stuck-loop](/fix/ai-stuck-loop)。
