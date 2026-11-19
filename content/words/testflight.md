---
title: TestFlight
slug: testflight
aka: ["TestFlight", "内测", "测试版", "beta", "内部测试", "外部测试"]
summary: 苹果官方的App内测工具，让朋友在真机上先试用你的App。
category: App
level: 6
related: [app-store, xcode, mini-program, pwa, deploy]
updated: 2026-11-18
---

## 一句话
TestFlight是苹果的官方测试分发工具。你把App上传，邀请别人，对方在手机上装一个TestFlight App，就能下载试用你的测试版，不用上架。

## 打个比方
像饭店正式开业前的试营业。只请熟人来吃，提意见，没问题再对外营业。

不贴切的地方：试营业不需要营业执照，TestFlight却需要你已经有付费的开发者账号，而且对外部测试者还要过一次简易审核。

## 你会在哪儿碰到它
- 在[Xcode](/words/xcode)里把App打包（Archive）并上传到App Store Connect，几分钟后出现在TestFlight页面。
- 内部测试：团队成员最多100人，需要是你开发者账号里的成员。
- 外部测试：最多10,000人，邀请链接即可加入，但首个版本需通过苹果的Beta App Review（官方页面所述）。
- 测试版有有效期，通常是上传后90天，到期后需要传新版本。
- 国内经常遇到上传卡住，和网络有关，可参考代理设置。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
请带我把这个iOS App上传到TestFlight，我是第一次做。
按顺序告诉我：1. 我在App Store Connect里要先手动建好哪些东西（App记录、Bundle ID）；2. Xcode里该怎么设版本号和签名；3. 怎么Archive并上传；4. 上传后怎么邀请朋友测试。
每一步说明我会在屏幕上看到什么，出现常见报错时怎么处理。
```

## 容易踩的坑
- 每次上传的构建号（build number）必须比上次大，否则会被拒收。
- 导出合规（加密）问题一直弹出：在配置里如实声明，AI能帮你加对应设置。
- 以为TestFlight测过了就能直接上架：还要走[App Store上架](/words/app-store)的完整提交。
- 测试者没收到：检查对方登录的Apple账号邮箱是否与邀请一致。
- 90天过期后用户突然打不开，记得提前发新版本。
