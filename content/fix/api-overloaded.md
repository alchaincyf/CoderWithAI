---
title: "报错「API Error: 529 Overloaded / 503 服务繁忙」怎么办"
slug: api-overloaded
summary: 服务端太忙，不是你的问题也不扣你额度。等几分钟重试，或用/model换个模型。
category: AI工具
match: ["529", "Overloaded", "overloaded_error", "503", "Service Unavailable", "The API is at capacity", "is experiencing high load"]
platform: [all]
updated: 2026-11-18
---

```error
API Error: Repeated 529 Overloaded errors. The API is at capacity — this is usually temporary. Try again in a moment. If it persists, check https://status.claude.com.
```

直接调用API时，返回的是：

```error
{"type":"error","error":{"type":"overloaded_error","message":"Overloaded"}}
```

有时还会看到：

```error
Opus is experiencing high load, please use /model to switch to Sonnet
```

## 人话：这是怎么回事

529表示AI服务那头暂时忙不过来了。Anthropic官方说明：高峰期所有用户的流量都很大时可能出现。它**不是你的用量上限**，不会占用你的额度，你的账号、密钥、代码都没问题。

503（Service Unavailable）是各种服务通用的「暂时不可用」，含义接近。对你来说处理方式一样：等一等，再试。

## 最快的解法：把这段发给AI

如果是你写的程序调用API时遇到，这样让AI加上稳健处理：

```prompt 复制给 Claude Code
我的程序调用AI接口时偶尔遇到 529 Overloaded / 503 错误。请帮我加上合理的重试处理：
1. 先检查我用的SDK是否已经自带重试（官方SDK默认会对连接错误、限流和5xx自动重试两次），不要重复造轮子，能用SDK的配置就用配置。
2. 如果需要自己写：只对 429、500、503、529 这类临时性错误重试；用指数退避加随机抖动；最多3到5次；用完仍失败时给用户一个友好提示，不要无限重试。
3. 不要对 401、400 这类错误重试。
4. 改完模拟一次失败来验证。
```

如果是Claude Code里弹出的，不需要问AI，直接看下面。

## 想自己动手的话

1. **等几分钟再试。** 在Claude Code里直接对它说「try again」或「继续」，消息还在对话里。
2. **看官方状态页。** 打开 `https://status.claude.com` 确认是否有已公布的故障。
3. **换模型。** 官方指出容量是按模型计算的。在Claude Code里运行：

```bash
/model
```

切换到另一个模型（比如从Opus换到Sonnet）再继续。

4. 如果错误持续很久、状态页又没有公告，可以用 `/feedback` 反馈。

用第三方中转或网关的人，错误可能来自中间那一层，请检查你配置的 `ANTHROPIC_BASE_URL` 指向的服务是否正常。

> [!warn] 写重试代码时一定要有次数上限。服务繁忙时无限重试，只会加重拥堵，还可能给你带来多余的费用。

## 下次怎么避免

关键业务的程序里做好重试和降级（比如备用模型）。别在流量高峰的时段做不能中断的批量任务，或者把批量任务切成可续跑的小段。

## 还不行？

如果提示里是 429 或 limit，那是另一类问题：看 [api-429](/fix/api-429) 和 [claude-usage-limit](/fix/claude-usage-limit)。网络连不上看 [fetch-failed](/fix/fetch-failed)。
