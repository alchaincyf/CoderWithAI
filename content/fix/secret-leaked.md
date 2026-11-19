---
title: 把API Key或密码推到了GitHub（GH013拦截）怎么办
slug: secret-leaked
summary: 密钥一旦上传就当作已泄露。第一步永远是去平台作废它，其次才是清理仓库。
category: Git
match: ["GH013", "Repository rule violations found", "GITHUB PUSH PROTECTION", "Push cannot contain secrets", "secret scanning", "leaked"]
platform: [all]
updated: 2026-11-18
---

```error
remote: error: GH013: Repository rule violations found for refs/heads/main.
remote:
remote: - GITHUB PUSH PROTECTION
remote:   Resolve the following violations before pushing again
remote:
remote:   - Push cannot contain secrets
```

不同密钥类型的提示细节不同，核心都是 `GH013` 和 `PUSH PROTECTION`。

## 人话：这是怎么回事

GitHub扫描到你要推送的内容里有密钥（API Key、令牌、密码），先拦住了。这是好事，说明它没到别人手里。

如果你**已经推送成功**了（尤其是公开仓库），要把这个密钥当作已经泄露。删掉文件、再提交一次都没用，因为Git历史里还留着旧版本，自动扫描程序会抓取公开仓库里的密钥。

所以顺序是：**先作废，再清理**。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我的git push被GitHub拦截了（GH013 push protection），说代码里有密钥。

请严格按这个顺序，每一步先解释再做：
1. 运行 git status 和 git log --oneline -10，再用 git grep 找出密钥在哪个文件、哪几次提交里。只告诉我文件名和行号，不要在回复里打印密钥本身。
2. 提醒我先去对应平台作废这个密钥并生成新的（你不能代我操作）。
3. 把密钥改成从环境变量读取，新建 .env，把 .env 加进 .gitignore，并提供 .env.example（只放变量名）。
4. 清理历史：如果密钥只在最近一次提交，用 git commit --amend；如果在更早的提交里，先告诉我方案和风险，我确认后再用 git rebase 或 git filter-repo。
5. 不要使用GitHub页面上的「Allow me to push this secret」绕过选项。
最后验证 git push 能通过，并且 git log -p 里搜不到旧密钥。
```

## 想自己动手的话

第一步，去签发这个密钥的平台（OpenAI、Anthropic、Supabase、云服务商等）的控制台，**删除或重置该密钥**，换新的。然后检查用量页面有没有不是你产生的调用。

第二步，让代码不再包含密钥，改用 [环境变量](/words/env-var)，并把 `.env` 写进 `.gitignore`。

第三步，从历史里移除。GitHub官方文档的做法：

```bash
git commit --amend --all
```

如果密钥只在最近一次提交里，改掉文件后用它覆盖那次提交，再 `git push`。

如果在更早的提交里，官方建议用交互式变基：

```bash
git rebase -i <最早含密钥的提交ID>~1
```

在编辑器里把那个提交前的 `pick` 改成 `edit`，改文件、`git add .`、`git commit --amend`、`git rebase --continue`，最后 `git push`。

> [!warn] 页面上那个「Allow me to push this secret」的绕过按钮别点，除非你百分百确定这是测试用的假密钥。点了等于主动把真密钥公开。另外，别把密钥写进前端代码里，用户打开浏览器就能看到。

## 下次怎么避免

项目一开始就建好 `.gitignore` 并写入 `.env`。让AI写代码时明确说一句「密钥一律从环境变量读取，不要写进代码」。

## 还不行？

已经成功推送到公开仓库的，除了作废密钥，还要检查该平台账单。清理历史后，仓库的旧提交可能仍被缓存或已被他人克隆，所以作废密钥才是真正的保险。相关概念见 [api-key](/words/api-key)。
