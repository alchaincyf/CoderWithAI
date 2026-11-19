---
title: "Hydration failed、文本内容不匹配怎么办（Next.js/React）"
slug: hydration-mismatch
summary: 服务器生成的页面和浏览器渲染的对不上。多半是时间、随机数或标签嵌套写错。
category: 页面问题
match: ["Hydration failed because the server rendered HTML didn't match the client", "Text content does not match server-rendered HTML", "Hydration failed", "hydrated but some attributes", "Warning: Text content did not match", "cannot be a descendant of"]
platform: [all]
updated: 2026-11-18
---

```error
Error: Hydration failed because the server rendered HTML didn't match the client. As a result this tree will be regenerated on the client.
```

也可能是：

```error
Warning: Text content did not match. Server: "10:23:01" Client: "10:23:02"
```

## 人话：这是怎么回事

Next.js会先在服务器上把页面生成好发给浏览器，浏览器再接手让它能点击，这一步叫hydration。接手时发现「服务器给的」和「我自己算的」不一样，就报这个错。

常见的不一样：页面里显示了当前时间或随机数；用了只有浏览器才有的东西（比如读取本地存储）；HTML标签嵌套不合规（段落里套了div）；浏览器的翻译插件改了页面文字。

页面往往还能用，但值得修，尤其别忽略它。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我的Next.js页面控制台报Hydration failed / Text content does not match server-rendered HTML。完整报错如下：

（在这里粘贴报错，包含Server: 和Client: 对比的那几行）

请先用人话解释是哪个组件、哪段内容在服务器和浏览器里不一致。
常见原因逐一排查：Date.now()/new Date()/Math.random()直接渲染、直接读window或localStorage、<p> 里嵌 <div>。
需要浏览器才有的值，改成在useEffect里设置。不要为了消除警告就到处加suppressHydrationWarning。
修完用无痕窗口访问（排除浏览器插件干扰），确认控制台不再报。
```

## 想自己动手的话

第一步，换一个无痕窗口打开页面。如果错消失了，是浏览器插件（翻译、广告拦截）改了页面，不是你的代码。

第二步，看报错里对比的两段文字，找到代码里渲染这段文字的位置。

第三步，对症处理：

- 显示时间、随机数：不要在渲染时直接算，先用占位，再在浏览器端更新。

```jsx
const [time, setTime] = useState(null)
useEffect(() => { setTime(new Date().toLocaleTimeString()) }, [])
```

这段让时间只在浏览器里设置，服务器输出的是空值，两边就一致了。

- 标签嵌套：`<p>`里不能放`<div>`，把外层改成`<div>`。

只有确实是「时间戳这类无法避免的差异」，才在那个元素上加`suppressHydrationWarning`，且只加在最小的元素上。

## 下次怎么避免

让AI写代码时，说明「涉及时间、随机数、本地存储的内容要放到useEffect里」。

## 还不行？

- 页面干脆白屏：看[页面空白](/fix/blank-page)。
- 部署到线上才出现：看[Vercel构建失败](/fix/vercel-build-failed)。
- 想了解开发者工具：[开发者工具](/words/devtools)。
