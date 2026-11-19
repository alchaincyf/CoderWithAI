---
title: "Vercel部署成功但打开是「404: NOT_FOUND」怎么办"
slug: vercel-404
summary: 部署成功却404，多是输出目录或根目录设错，或单页应用刷新子路径没配重写。
category: 部署上线
match: ["404: NOT_FOUND", "NOT_FOUND", "Code: NOT_FOUND", "404 NOT_FOUND", "This page could not be found"]
platform: [all]
updated: 2026-11-18
---

```error
404: NOT_FOUND
Code: NOT_FOUND
ID: hnd1::abcde-1731900000000-0123456789ab
```

## 人话：这是怎么回事

[Vercel](/words/vercel) 说部署是成功的，但你访问的地址它找不到对应的页面或文件。官方文档列出的原因包括：URL写错了、部署被删除了、没有权限访问，或部署本身不可用。

对用AI做的项目，最常见的其实是两种：**Vercel不知道你的网页文件放在哪**（输出目录、根目录设错），或者**单页应用（SPA）刷新子页面就404**。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的网站部署到Vercel后，打开显示 404: NOT_FOUND（Code: NOT_FOUND）。部署状态是Ready。

请帮我诊断：
1. 看 package.json、框架配置（vite.config / next.config 等）和 vercel.json，告诉我这是什么框架、构建后的文件输出到哪个目录。
2. 对比我在Vercel项目设置里的 Framework Preset、Output Directory、Root Directory，我会告诉你当前的值：（把你在Settings里看到的填在这里）
3. 判断是不是单页应用刷新子路径404，如果是，给出 vercel.json 重写规则的方案。
4. 改完后在本地 npm run build，确认输出目录里有 index.html，再告诉我要提交什么、在Vercel上改什么。
```

## 想自己动手的话

先区分：打开首页（`/`）就404，还是只有刷新或访问子页面才404？

首页就404，去Vercel项目的 Settings，检查这几项：

- Framework Preset 是否选对了框架。
- Output Directory：Vite项目默认输出到 `dist`，有些老项目是 `build`。填错就会找不到 `index.html`。
- Root Directory：如果代码在仓库的某个子文件夹里，要指向那个文件夹。

只有子页面404（比如直接打开 `/about`），而首页正常，是单页应用的路由问题。React、Vue这类项目需要告诉服务器「所有路径都交给 index.html」。在项目根目录建 `vercel.json`：

```json
{
  "rewrites": [{ "source": "/(.*)", "destination": "/index.html" }]
}
```

这段配置表示任何路径都返回 `index.html`，让前端路由自己处理。Next.js项目自带路由，不需要它。

改完提交推送，等Vercel重新部署，再用无痕窗口访问确认。

## 下次怎么避免

第一次部署后，点开首页、再手动输入一个子页面地址各试一次。部署前在本地跑 `npm run build`，确认输出目录里有你要发布的文件。

## 还不行？

页面能开但是空白，看 [blank-page](/fix/blank-page)。构建本身失败，看 [vercel-build-failed](/fix/vercel-build-failed)。用自己的域名打开才404，看 [domain-not-working](/fix/domain-not-working)。部署概念见 [deploy](/words/deploy)。
