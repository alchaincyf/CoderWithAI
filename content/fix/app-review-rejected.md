---
title: App Store审核被拒（4.3重复App、2.1信息不全、5.1.1隐私）怎么办
slug: app-review-rejected
summary: 先读拒绝信里的条款号，对照苹果审核指南逐条回应。多数是可以修复的，别急着重复提交。
category: 手机App
match: ["Guideline 4.3", "Guideline 2.1", "Guideline 5.1.1", "App Review", "rejected", "Resolution Center", "Spam", "App Completeness"]
platform: [all]
updated: 2026-11-18
---

```error
Guideline 2.1 - Performance - App Completeness

We were unable to review your app because ...
```

不同条款的标题格式类似：`Guideline 条款号 - 分类 - 标题`。具体措辞每封信不同，请以你的App Store Connect里Resolution Center的原文为准。

## 人话：这是怎么回事

被拒不等于App死刑。每封拒绝信开头都有一个**条款号**，对应苹果《App Store审核指南》里的某一条。你要做的是找到那一条，看它要求什么，修改后重新提交（或在Resolution Center回复说明）。

下面是新手用AI做App最容易遇到的几条（条款号和内容已对照苹果官方审核指南页面核实，指南会更新，以官网为准）：

- **2.1 App Completeness（完整性）**：提交的必须是最终版本，元数据和网址要完整可用，占位文字、空网站要清掉；App要在真机上测试过；如果有登录，必须提供**演示账号**并保证后端服务开着；内购项目要在App里能找到。闪退和功能不通也归在这里。
- **4.3 Spam**：4.3(a)不要为同一个App创建多个Bundle ID；4.3(b)不要提交与市面上已有的App没有区别的东西，在热门App类别里换皮做变体会被拒。AI批量生成的模板App很容易撞这条。
- **5.1.1 Data Collection and Storage（数据收集）**：(i)必须在App Store Connect和App内提供隐私政策链接，说明收集什么数据、怎么用；(v)如果App支持创建账号，必须在App内提供**删除账号**的功能。
- **4.2 Minimum Functionality**：只是套壳网页、没什么独特功能的App可能被拒。
- **3.1.1 In-App Purchase**：解锁功能必须用苹果的内购，不能自己搞激活码。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的iOS App被App Store审核拒绝了，拒绝信原文如下：

（把Resolution Center里的完整原文粘贴到这里）

请：
1. 找出条款号，用大白话解释苹果具体不满意什么。
2. 对照我的项目代码，检查我是否真的存在这个问题（比如缺隐私政策链接、没有账号删除功能、有占位文本、权限说明文案不清楚）。
3. 列出需要修改的清单，改完后帮我写一段给审核团队的回复，客气、具体、说明做了什么改动。
4. 不要凭记忆引用条款内容，以我贴的拒绝信为准。涉及我需要在App Store Connect里点击的操作，写成步骤让我自己做。
```

## 想自己动手的话

1. 在App Store Connect的Resolution Center读完整原文，记下条款号和附带的截图或说明。
2. 打开苹果官方的App Store审核指南，找到对应条款读原文。
3. 修改后重新构建、提交新版本，或者在Resolution Center直接回复说明（有些情况只需补充信息，不用重新提交）。
4. 觉得审核理解错了，可以礼貌地回复并附上证明；也可以通过App Store Connect里的申诉渠道提出。

> [!warn]不要原样重复提交，也不要用小号、换Bundle ID的方式规避，这反而可能触发4.3或让账号受到影响。

## 下次怎么避免

提交前对照指南自查：隐私政策链接、账号删除、演示账号、没有占位内容、没有崩溃。让App有自己独特的功能，而不是模板换皮。提交时在备注里写清楚怎么体验核心功能。

## 还不行？

遇到的是签名和打包问题，看[xcode-signing](/fix/xcode-signing)。TestFlight的概念见[testflight](/words/testflight)，上架整体流程见[app-store](/words/app-store)。
