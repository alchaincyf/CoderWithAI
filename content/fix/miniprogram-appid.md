---
title: 微信小程序「appid不合法/无效的AppID / invalid appid」怎么办
slug: miniprogram-appid
summary: AppID填错、用了游客模式，或服务端appid与secret不匹配。逐处核对来源，并且AppSecret不能放前端。
category: 手机App
match: ["invalid appid", "40013", "appid不合法", "AppID 不合法", "无效的AppID", "touristappid", "appid missing"]
platform: [all]
updated: 2026-11-18
---

```error
{"errcode":40013,"errmsg":"invalid appid"}
```

在开发者工具里，还可能看到「AppID不合法」或「无效的AppID」这类提示，措辞随工具版本不同。

> 说明：本条的报错文案请以微信官方文档为准，这里没有官方逐字样本。`errcode 40013 / invalid appid`来自服务端接口的实际返回（社区多处出现），开发者工具里的中文提示请以你屏幕上的为准。下面的排查思路有效，但措辞别当成官方原文。

## 人话：这是怎么回事

AppID是小程序的身份证号。报错说明：你提供的AppID，微信不认。分两个场景：

- **开发者工具里**：项目配置里的AppID填错、复制多了空格，或者还在用「游客模式」（`touristappid`）这个占位值。游客模式能看效果，但很多能力（上传、真机、部分接口）用不了。
- **服务端调接口时**（返回`40013 invalid appid`）：服务端使用的appid和secret不是同一个小程序，或者混用了公众号的appid，或者拿到的access_token有问题。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的微信小程序出现invalid appid / appid不合法 的报错：

（粘贴报错原文，并说明是在开发者工具里，还是服务端接口返回的）

请：
1. 检查项目根目录project.config.json的appid字段，以及manifest.json（若是uni-app）里的mp-weixin.appid，看是否一致、是否还是touristappid、有没有多余空格。
2. 如果是服务端报40013：检查服务端使用的appid和appsecret是否来自同一个小程序，是否误用了公众号的appid，获取access_token的逻辑是否正确。
3. 确认AppSecret只存在于服务端的环境变量里，没有出现在小程序前端代码或GitHub仓库中。
4. 不要打印AppSecret的内容。
```

## 想自己动手的话

1. 去[微信公众平台](https://mp.weixin.qq.com)登录你的小程序账号，在**开发管理 > 开发设置**里找到AppID（小程序ID），重新复制，注意不要带空格。
2. 在微信开发者工具里，**详情 > 基本信息**查看并核对项目的AppID，需要时重新填写。也可检查`project.config.json`里的`appid`字段，把`touristappid`换成你的真实AppID。
3. 项目是用测试号还是正式账号？两者的AppID不同，别混用。
4. 服务端调用：确认appid和secret配成同一对，小程序的appid不要和公众号的搞混。
5. 修改后重新编译，必要时关闭开发者工具重新导入项目。

> [!warn] AppSecret（小程序密钥）相当于这个小程序的密码，只能放在你的服务端，绝不能写进小程序代码或提交到GitHub。泄露后应在后台重置，见[secret-leaked](/fix/secret-leaked)。

## 下次怎么避免

新建项目时直接填真实AppID，不用游客模式。服务端的appid和secret放进[环境变量](/words/env-var)，在`.env.example`里记下变量名，别写死在代码里。

## 还不行？

请求接口被拦截看[miniprogram-domain](/fix/miniprogram-domain)。对小程序整体概念见[mini-program](/words/mini-program)。仍然找不到问题，把脱敏后的配置文件（隐去secret）发给AI，或到微信开放社区发帖求助。
