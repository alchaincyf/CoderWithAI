---
title: JSON
slug: json
aka: ["JSON", "package.json", "配置文件", "数据格式", "JSON.parse"]
summary: 一种用文字表示数据的通用格式，程序之间传数据、存配置都用它。
category: 基础
level: 2
related: [api, npm, markdown, database, env-var]
updated: 2026-11-18
---

## 一句话
JSON是一种写数据的格式：用大括号、方括号、冒号、引号，把「名字对应值」整整齐齐地写出来。

## 打个比方
像一张填好的表格：姓名：小王，年龄：28，爱好：钓鱼、摄影。JSON把这种「项目：内容」写成程序能读懂的样子。

不贴切的地方：表格你可以随手涂改，JSON的格式要求极严，少一个逗号整份就读不了。

## 你会在哪儿碰到它
长这样：

```json
{
  "name": "小王",
  "age": 28,
  "hobbies": ["钓鱼", "摄影"]
}
```

- package.json、tsconfig.json这类配置文件，都是JSON。
- 调用[API](/words/api)时，返回的数据绝大多数是JSON。
- 让AI帮你整理数据、导出结果，它常会输出JSON。
- 文件后缀是 .json。

## AI提到它时，你该怎么接
```prompt
请把下面这些内容整理成JSON格式，每一项包含name、price、description三个字段。并检查JSON语法是否正确，确保我可以直接保存成 .json文件使用。（粘贴内容）
```

```prompt
这份JSON文件被程序报错读不了（粘贴报错和文件内容）。请指出具体是哪一行哪个符号有问题，修好后给我完整的文件。
```

## 容易踩的坑
- 最后一项后面多写了逗号。JSON不允许，这是最常见的错误。
- 用了中文引号或单引号。JSON里字符串只能用英文双引号。
- 往JSON里写注释。标准JSON不支持注释，写了会报错。
- 手动编辑package.json时改坏。尽量让AI改。
