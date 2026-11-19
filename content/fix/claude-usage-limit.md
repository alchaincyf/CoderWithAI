---
title: Claude Code提示用量上限（You've hit your session limit）怎么办
slug: claude-usage-limit
summary: 订阅额度按5小时窗口和每周窗口计算，用完要等重置。可以等、开额外用量，或省着用。
category: AI工具
match: ["You've hit your session limit", "You've hit your weekly limit", "Usage limit reached", "usage limit reached", "You've hit your Opus limit", "You've hit your Sonnet limit", "/rate-limit-options"]
platform: [all]
updated: 2026-11-18
---

```error
You've hit your session limit · resets 3:45pm
You've hit your weekly limit · resets Mon 12:00am
You've hit your Opus limit · resets 3:45pm
```

等待重置时，界面上还可能出现：

```error
Usage limit reached · limit resets 3:45pm
```

## 人话：这是怎么回事

用订阅（Pro、Max等）登录Claude Code，你有一份滚动的使用额度。按官方帮助中心的说法，包含5小时的会话额度和每周额度，Claude网页版和Claude Code共用。用完了就被暂停，直到提示里那个重置时间。这不是账号出问题，也不是封号。

时效提醒：具体额度大小和套餐规则会调整，截至2026年11月，以官方帮助中心和`/usage`显示为准。

三种情况略有不同：

- session limit和weekly limit：所有模型共用，换模型没用。
- Opus limit或Sonnet limit：只限那个模型家族，用`/model`换到别的家族可以继续。

## 最快的解法：把这段发给AI

用量用完时Claude Code自己没法回答你。可以换一个AI（比如网页版对话）帮你规划，或者等重置后发给Claude Code：

```prompt 复制给 Claude Code
我的Claude Code用量消耗太快了，请帮我诊断并省着用：
1. 告诉我如何用/usage查看哪些行为（子代理、MCP、长上下文）占用最多。
2. 检查CLAUDE.md / AGENTS.md是否太长，把只在特定任务用到的内容建议拆成按需加载。
3. 提醒我什么时候该/clear开新对话，什么时候该/compact。
4. 大任务先用计划模式出方案，再执行，避免返工。
```

## 想自己动手的话

按官方的做法，选一种：

1. **等。**提示里写了重置时间。官方说明在v2.1.234及以上的版本，订阅登录的交互会话在用量用完时可以原地等待，重置后自动接着做。想控制这个行为，运行`/rate-limit-options`选「Wait here, then continue automatically」或取消。
2. **开额外用量。**运行`/usage-credits`，Pro、Max等套餐会在浏览器打开设置页，可以打开用量额度（Usage credits）继续使用，按量计费。团队版则向管理员申请。
3. **换模型。**如果提示的是Opus或Sonnet limit，用`/model`换到别的模型家族。
4. **看用量。**运行`/usage`查看额度条和重置时间。

排查一个常见陷阱：如果你的环境里设置过`ANTHROPIC_API_KEY`，Claude Code会优先用API密钥而不是订阅，产生按量计费。想改回订阅，运行`/login`并选订阅账号。

> [!warn]用量高低和你说的话多少不成正比。Claude Code每次请求都会带上整段对话，所以一个开了一整天的长对话，问一句小问题也很耗额度。

## 下次怎么避免

不相关的任务之间用`/clear`开新对话；提示词写具体，别让AI漫无目的地翻整个项目；轻量任务用小一点的模型。对话太长的问题见[context-full](/fix/context-full)。

## 还不行？

提示的是429或529而不是limit，那是另一回事，看[api-429](/fix/api-429)和[api-overloaded](/fix/api-overloaded)。账户计费问题，官方建议通过产品内的Get help联系支持。
