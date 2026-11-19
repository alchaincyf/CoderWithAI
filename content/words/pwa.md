---
title: PWA（能装到桌面的网页）
slug: pwa
aka: ["PWA", "Progressive Web App", "渐进式网页应用", "添加到主屏幕", "manifest", "Service Worker"]
summary: 一个能添加到手机桌面、像App一样打开的网页。
category: App
level: 6
related: [https, frontend, deploy, app-store, mini-program]
updated: 2026-11-18
---

## 一句话
PWA是一个普通网站，加上一份说明文件，就能被「安装」到手机或电脑桌面，点图标全屏打开，样子像App，却不用上架商店。

## 打个比方
像把一家餐厅的外卖链接钉在手机桌面上。点一下就进，不用去商场找，也不用审核。

不贴切的地方：它终究是网页，能用的手机能力比原生App少，在iPhone上限制更多，比如没有「安装」按钮，要自己用分享菜单添加。

## 你会在哪儿碰到它
- AI说「先做成PWA，省去上架」，会生成一个`manifest.json`文件和各种尺寸图标。
- 在Chrome地址栏旁出现「安装」图标。
- MDN文档的要求：必须用[HTTPS](/words/https)（本地开发时用localhost除外）；Chromium浏览器要求manifest里有名称、192与512像素图标、start_url、display等；Service Worker对于「能安装」并非必需，主要用来支持离线。
- iPhone上要通过Safari等浏览器的分享菜单添加；iOS 16.4起其他浏览器也支持，但没有自定义安装弹窗功能。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
请把这个网站改造成可以安装的PWA：
1. 添加manifest.json，名称、图标（192和512）、start_url、display: standalone；
2. 先不做离线缓存，只保证可以安装；
3. 告诉我怎么在Chrome里检查它是否满足安装条件（用开发者工具的Application面板），以及在iPhone Safari里怎么添加到主屏幕。
```

## 容易踩的坑
- 没有HTTPS就装不了：上线域名要有证书。
- 离线缓存用不好，用户一直看到旧版本：改了内容没生效。第一版先不做缓存。
- 以为PWA能做所有App能做的事：推送、蓝牙等能力在不同系统上支持不一致，需要先查。
- 图标没做对尺寸，桌面上图标模糊或缺失。
