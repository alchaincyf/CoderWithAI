---
title: Webhook
slug: webhook
aka: ["Webhook", "回调", "Callback", "网络钩子"]
summary: 对方有事时主动敲你的门通知你，而不是你一直去问。
category: 连接
level: 7
related: [payment, api, localhost, server, serverless]
updated: 2026-11-18
---

## 一句话
Webhook是一个你提前告诉对方的网址。对方那边一有事（比如用户付款成功），就向这个网址发一条消息通知你。

## 打个比方
像快递送到时打你电话，而不是你每五分钟去查一次物流。你留下的电话号码，就是Webhook地址。

不贴切的地方：电话没接通对方会再打，Webhook也会重试，但如果你的服务一直没响应，对方最终会放弃。

## 你会在哪儿碰到它
- 接[在线收款](/words/payment)时，AI会说「支付成功后要靠webhook来确认订单」。
- GitHub、Stripe、飞书机器人、Notion等都能配置Webhook。
- 代码里出现 `/api/webhook`，或在服务商后台有一栏「Webhook URL」。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
请给项目加一个接收Stripe支付结果的webhook接口。
要求：1. 必须校验签名，用环境变量里的signing secret；2. 同一个事件被重复推送时不能重复发货（做幂等处理）；3. 先告诉我本地怎么测试（比如用Stripe CLI把事件转发到localhost），我跑通后再上线；4. 记录每次收到的事件，方便排查。
```

## 容易踩的坑
- 本地的[localhost](/words/localhost)地址外部访问不到，对方推不进来。本地测试要用官方提供的转发工具或隧道服务。
- 不校验来源：任何人都可以伪造一条「已付款」请求。必须验签。
- 处理太慢：多数服务商要求几秒内返回成功，先回复收到，再慢慢处理。
- 重复通知：同一事件可能推送多次，不做幂等会重复发货、重复发邮件。
- 上线后忘了把后台的Webhook网址从测试地址改成线上地址。
