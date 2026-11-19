---
title: SQL
slug: sql
aka: ["SQL", "查询语句", "SELECT"]
summary: 和数据库对话用的语言，用来查、加、改、删数据。
category: 数据
level: 4
related: [database, crud, supabase, backend]
updated: 2026-11-18
---

## 一句话
SQL是跟[数据库](/words/database)说话的语言。几乎都是英语句子的样子：从哪张表里，挑出符合什么条件的数据。

## 打个比方
像给图书管理员写的借书条：「去『小说』书架，拿出所有作者是张三、2020年以后出版的书，按出版时间排好」。管理员照着做，不用你自己去翻。

不贴切的地方：管理员会反问你，SQL不会。你写错一个条件，它照样执行。

## 你会在哪儿碰到它
- AI给你一段这样的东西，让你粘到数据库后台里运行：

```sql
SELECT name, email FROM users WHERE created_at > '2026-01-01' ORDER BY created_at DESC;
```

- [Supabase](/words/supabase)里的SQL Editor；项目里的 `.sql` 文件、`migrations/` 文件夹。
- 报错里出现 `syntax error at or near`，一般是某句SQL写错了。

## AI提到它时，你该怎么接
大多数时候AI会替你在代码里写好并运行。需要你手动执行时，先让它解释。

```prompt 复制给 Claude Code
你给我的这段SQL我要手动在Supabase的SQL Editor里运行。
请先逐行用人话解释它会做什么，它会不会修改或删除已有数据，需要备份吗。
如果有风险，请改成先用SELECT预览会影响哪几行，我确认后再执行真正的修改。
```

## 容易踩的坑
- `DELETE` 或 `UPDATE` 忘了写 `WHERE`：会作用到整张表的所有行。执行前让AI先用 `SELECT` 预览。
- `DROP TABLE` 是删表，通常无法撤销。没备份别执行。
- 不同数据库方言略有差别（SQLite、MySQL、PostgreSQL）。AI给的语句要先确认是按你用的那一种写的。
- 把用户输入直接拼进SQL字符串，会被注入攻击。让AI用参数化查询，而不是自己拼。
