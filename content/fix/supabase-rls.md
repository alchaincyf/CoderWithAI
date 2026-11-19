---
title: Supabase报错「new row violates row-level security policy」怎么办
slug: supabase-rls
summary: 表开了行级安全（RLS）但没有允许你写入的策略。写一条正确的策略，别图省事把RLS关掉。
category: 数据与接口
match: ["new row violates row-level security policy", "row-level security", "42501", "violates row-level security policy for table", "RLS"]
platform: [all]
updated: 2026-11-18
---

```error
{
  "code": "42501",
  "details": null,
  "hint": null,
  "message": "new row violates row-level security policy for table \"todos\""
}
```

## 人话：这是怎么回事

[Supabase](/words/supabase)的[数据库](/words/database)表可以开启RLS（行级安全，Row Level Security）：谁能读、谁能写哪几行，由「策略」决定。一旦开启，**没有策略就什么都不允许**。官方说明：没有策略时，读取返回空，写入会被拒绝。

你看到的报错就是：你要新增的这一行，没有任何策略批准。它是个保护机制在正常工作，数据没有坏。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的Supabase表todos在插入数据时报错：new row violates row-level security policy for table "todos"（code 42501）。

请：
1. 先检查这张表的结构和已有的RLS策略（可以看supabase/migrations里的SQL，或让我去控制台复制）。
2. 判断原因：是没有INSERT策略？策略条件不满足？还是用户没登录导致auth.uid()为null？还是插入时没有带上user_id？
3. 给出最小范围的修复：为authenticated角色写一条INSERT策略，用with check ((select auth.uid()) = user_id)，并让user_id默认等于auth.uid()。同样检查SELECT/UPDATE/DELETE策略是否合理。
4. 严禁用"关闭RLS"或"改用secret key（旧名service_role）"来绕过。secret key绝不能出现在前端代码里。
SQL先给我看，我确认后再执行。
```

## 想自己动手的话

在Supabase控制台的Authentication > Policies里可以看到每张表的策略。最常见的修法是增加一条插入策略（用SQL编辑器执行，`todos`和`user_id`换成你的）：

```sql
create policy "Users can insert their own todos"
on todos for insert
to authenticated
with check ( (select auth.uid()) = user_id );
```

意思是：已登录用户可以插入新行，但新行的`user_id`必须是他自己。这是官方文档的写法。

读取则需要对应的select策略：

```sql
create policy "Users can view their own todos"
on todos for select
to authenticated
using ( (select auth.uid()) = user_id );
```

还有一个高发原因：**用户没登录**。未登录时`auth.uid()`是空的，任何依赖它的策略都不通过。先确认前端是登录后才调用的。

> [!warn]网上最常见的「解法」是`alter table ... disable row level security`，或在前端改用secret key（旧名service_role）。这两种都会让任何人能读写你的整张表。secret key（service_role）会绕过所有RLS，官方明确要求它只能放在服务端，不能出现在浏览器里。

## 下次怎么避免

建表时就让AI同时写好RLS和四类策略（select/insert/update/delete），并在`user_id`上设默认值。每个新表都问一句「哪些人能读、哪些人能写」。

## 还不行？

如果查询没报错但返回空数组，也是RLS在起作用（缺少select策略）。登录相关的问题见[auth](/words/auth)。可以把表结构和现有策略一起发给AI诊断。
