---
title: 微信小程序报「不在以下request合法域名列表中」怎么办
slug: miniprogram-domain
summary: 小程序只能请求在后台登记过的域名。去公众平台配置request合法域名，开发时可临时关闭校验。
category: 手机App
match: ["不在以下 request 合法域名列表中", "url not in domain list", "request:fail url not in domain list", "合法域名", "服务器域名", "不校验合法域名"]
platform: [all]
updated: 2026-11-18
---

```error
https://api.example.com 不在以下 request 合法域名列表中，请参考文档：https://developers.weixin.qq.com/miniprogram/dev/framework/ability/network.html
```

真机或运行时可能看到：

```error
request:fail url not in domain list
```

## 人话：这是怎么回事

[微信小程序](/words/mini-program)出于安全考虑，只允许请求**提前在小程序后台登记过**的域名。你的代码请求了一个没登记的地址（可能是你的后端，也可能是第三方接口），微信就拦住了。

开发阶段可以临时关闭检查，但上线后必须正式配置，否则用户那边一样报错。

> 说明：本条的操作路径和域名要求，来自微信开发者工具的实际界面和社区文档整理，具体规则请以微信官方文档和你看到的界面为准。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的微信小程序请求接口时报错：xxx不在以下request合法域名列表中。

请帮我：
1. 搜索项目里所有wx.request（或封装的请求方法）用到的地址，列出需要登记的域名清单，注意：只写域名，不要带路径，并标出哪些不是https。
2. 检查有没有用IP地址、localhost或http开头的地址，这些不能在正式环境使用，要换成已备案的https域名。
3. 告诉我在微信公众平台小程序后台的哪里配置，我自己去操作。
4. 如果请求的是第三方接口，建议改成经过我自己后端中转，这样只要登记一个域名，也不会把密钥放进小程序。
```

## 想自己动手的话

正式配置：登录[微信公众平台](https://mp.weixin.qq.com)的小程序后台，进入**开发管理 > 开发设置 > 服务器域名**，在request合法域名里添加你的域名。常见要求：

- 必须是`https`，需要有效证书（见[https](/words/https)）。
- 只能填域名，不带路径，不能用IP地址或localhost。
- 在中国大陆运营的小程序，域名一般需要完成[ICP备案](/words/icp-filing)。
- 域名要和代码里请求的完全一致，`api.example.com`和`www.example.com`是两个域名，需要分别添加。

配置后重新编译，必要时重启开发者工具。

开发调试时临时跳过：在微信开发者工具右上角点**详情 > 本地设置**，勾选「不校验合法域名、web-view（业务域名）、TLS版本以及HTTPS证书」。这个设置只对开发者工具和预览有效。

> [!warn]勾选「不校验」只是临时办法。社区里常见的坑是：开发时一切正常，发布体验版或正式版后立刻报同样的错，因为线上没有这个开关。上线前务必在后台配好。

## 下次怎么避免

写小程序一开始就规划好后端域名，备案、证书、后台登记一次办完。让AI把所有请求地址集中在一个配置文件里，方便核对。

## 还不行？

域名配置了还是报错，检查协议（http/https）、端口、拼写，以及是否保存生效。AppID相关问题看[miniprogram-appid](/fix/miniprogram-appid)。证书问题看[ssl-certificate-error](/fix/ssl-certificate-error)。
