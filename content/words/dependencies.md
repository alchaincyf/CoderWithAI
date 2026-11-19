---
title: 依赖
slug: dependencies
aka: ["Dependency", "Dependencies", "devDependencies", "package-lock.json", "版本号"]
summary: 你的项目借用的别人写好的代码包，清单写在package.json里。
category: 基础
level: 2
related: [npm, node-modules, nodejs, build, branch]
updated: 2026-11-18
---

## 一句话
依赖就是你的项目要靠别人的哪些代码才能跑起来。项目本身只写自己的部分，其余借用现成的包。

## 打个比方
做一道菜用了酱油、面粉、鸡蛋，这些是你的「依赖」，不是你自己种的。菜谱得列出来，缺一样，做不成。

不贴切的地方：酱油里还有大豆、盐，这些二级食材你也要，但你不关心。依赖也是一层套一层，一个包会带来几十个包。

## 你会在哪儿碰到它
- 打开package.json，里面有dependencies（运行时需要）和devDependencies（只在开发时需要）两块，每项后面是版本号，例如 ^19.0.0。
- 同时多一个package-lock.json（或pnpm-lock.yaml），它把每个包的精确版本锁住，保证你和别人装出来的一样。
- AI说「我需要装一个依赖」，就是用npm安装某个包，见[npm](/words/npm)。
- 报错里出现peer dependency、ERESOLVE，是说两个包对第三个包的版本要求冲突。

## AI提到它时，你该怎么接
```prompt
你刚说要加一个新依赖。请告诉我：它是做什么的、有多少人在用、是否有更轻的替代，或者能不能不装、自己写几行代码解决。
```

```prompt
安装依赖时报版本冲突（粘贴报错）。请先解释冲突的原因，再给出风险最低的解决办法，不要直接用强制安装的参数。
```

## 容易踩的坑
- 装得太多。每个依赖都是以后可能出问题的点，也让构建变慢。
- 不要手动改lock文件，也不要删了它再重装，除非AI说明原因，否则版本可能意外升级。
- 升级依赖导致项目崩。升级前让AI先看更新说明，并在新分支里试，见[分支](/words/branch)。
- 把依赖和node_modules混为一谈：依赖是清单，[node_modules](/words/node-modules)是按清单下载下来的实物。
