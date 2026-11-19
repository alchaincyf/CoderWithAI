---
title: 上下文窗口
slug: context-window
aka: ["Context Window", "上下文", "context", "上下文长度", "compact", "对话太长"]
summary: AI一次能同时「记住并看到」的内容上限。
category: AI
level: 0
related: [token, llm, agents-md, subagent, prompt]
updated: 2026-11-18
---

## 一句话
上下文窗口，是[大模型](/words/llm)在回答时一次能看到的全部内容：你的提示词、之前的对话、读过的文件、工具的输出，加起来用[Token](/words/token)衡量，有上限。

## 打个比方
像一张书桌。桌子上摊开的东西，AI才能同时看到。桌子满了，就得收走一些，被收走的它就不记得了。

不贴切的地方：人收走的东西自己还有印象，AI收走的就是真的没有了，除非被总结后留下一个摘要。

## 你会在哪儿碰到它
- 对话很长之后，AI开始忘记前面说过的约定、重复犯同一个错、变得「笨」。
- Claude Code里有几个相关命令：`/context`看当前占用，`/compact`把前面的内容压缩成摘要，`/clear`清空开始新话题。接近上限时它也会自动压缩较早的历史（官方文档称为auto-compaction）。
- 看到「context low」「conversation compacted」之类的提示。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
这个会话已经很长了，你好像开始忘记前面的约定。
请先总结：目前已完成的事、还没完成的事、我们定下的所有规则和重要文件路径，写成一份不超过20行的交接说明。
我会开一个新会话，把这份说明贴过去继续。
```

## 容易踩的坑
- 一个会话从头做到尾，什么都往里塞：越到后面越不准。每个独立任务用一个新会话。
- 重要规则只在对话里说了一次：会被压缩掉。长期有效的规则写进[AGENTS.md或CLAUDE.md](/words/agents-md)。
- 把整个项目丢给AI读：占掉大量窗口。让AI只读相关文件，或交给[子代理](/words/subagent)去查。
- 窗口大不等于不会忘：内容越多，AI越可能漏掉中间的细节。
