---
title: Supabase
slug: supabase
aka: ["Supabase", "BaaS", "后端即服务"]
summary: 一个现成的在线数据库加登录系统，让你不用自己搭后端。
category: 数据
level: 4
related: [database, auth, sql, api-key, backend]
updated: 2026-11-18
---

## 一句话
Supabase是一项在线服务：给你一个托管好的PostgreSQL[数据库](/words/database)，外加[登录注册](/words/auth)、文件存储，并自动生成可以直接调用的接口。

## 打个比方
像租了一间带前台和保险柜的现成办公室，而不是自己买地盖楼。数据库是保险柜，登录系统是前台，你只管把业务摆进去。

比方不完美的地方：房子是别人的，服务停了、改价了、国内访问慢了，你都得跟着受影响。

## 你会在哪儿碰到它
- 你想做「用户能注册、能保存自己的数据」的网站时，AI常推荐它。
- 项目里出现`@supabase/supabase-js`、`SUPABASE_URL`、`anon key`、`service_role`。
- 在supabase.com的网页后台里能看到Table Editor（像表格一样看数据）和SQL Editor。

## AI提到它时，你该怎么接
```prompt 复制给 Claude Code
请帮我把这个项目接上Supabase：用户可以邮箱注册登录，每个用户只能看到和修改自己的数据。
要求：1.先告诉我需要我在supabase.com后台手动做哪几步，以及要把哪些值填进.env；2.建表并开启行级安全策略（RLS）；3.写完后教我怎么用两个不同账号验证「看不到对方的数据」。
不要把service_role密钥放进前端代码。
```

## 容易踩的坑
- 密钥分两种：`anon`可以放在前端，但前提是你配好了行级安全（RLS）；`service_role`权限极大，只能放服务端，泄露等于数据库对外敞开。
- 没开RLS：表默认对谁都可读写的风险很常见，上线前一定让AI检查每张表的策略。
- 免费额度有限制：官方定价页写明免费版2个活跃项目、每项目500MB数据库，一周不活跃会被暂停，Pro起价每月25美元（截至2026年11月，以supabase.com/pricing为准）。
- 国内访问可能不稳定，需要在上线前用国内网络实测一次。
