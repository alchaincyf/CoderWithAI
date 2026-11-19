---
title: Git合并冲突（文件里出现<<<<<<< HEAD）怎么办
slug: git-merge-conflict
summary: 两个版本改了同一处，Git不敢替你选。保留需要的内容、删掉标记、提交就行。
category: Git
match: ["CONFLICT (content)", "Merge conflict in", "Automatic merge failed", "<<<<<<< HEAD", "fix conflicts and then commit the result", "Resolve all conflicts manually"]
platform: [all]
updated: 2026-11-18
---

```error
Auto-merging index.html
CONFLICT (content): Merge conflict in index.html
Automatic merge failed; fix conflicts and then commit the result.
```

文件里会多出这样的标记：

```error
<<<<<<< HEAD
<h1>欢迎来到我的网站</h1>
=======
<h1>Welcome</h1>
>>>>>>> feature-english
```

## 人话：这是怎么回事

两个版本（比如你的分支和别人的分支，或者你本地和GitHub上的）同时改了同一个文件的同一块地方。Git分不出该留哪个，就把两边都放进文件里，用标记框出来，让你来选。

`<<<<<<< HEAD`到`=======`之间是**你当前这边**的内容，`=======`到`>>>>>>>`之间是**对方那边**的内容。冲突是正常现象，没有任何东西丢失。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code
我在合并时遇到了冲突：Automatic merge failed，文件里有 <<<<<<< HEAD标记。

请先不要急着改。按顺序做：
1. 运行`git status`，列出所有冲突文件。
2. 对每个冲突文件，用大白话告诉我：两边各改了什么、各自的意图可能是什么、你建议留哪边或怎么合并，为什么。
3. 等我确认后再修改文件，把所有 <<<<<<<、=======、>>>>>>> 标记清除干净。
4. 修改后运行项目的构建或测试，确认没坏，再`git add`和`git commit`。
如果我说算了，就运行`git merge --abort`回到合并之前。
```

## 想自己动手的话

```bash
git status
```

列出带`both modified`的文件，那就是冲突文件。

打开文件，找到`<<<<<<<`。决定保留哪边（或两边都要、手工融合），然后把三行标记**连同不要的那一半**删掉。保存后：

```bash
git add index.html
```

告诉Git这个文件的冲突已经处理了（换成你的文件名）。

```bash
git commit
```

完成合并。如果是`git pull --rebase`引起的，这里改用`git rebase --continue`。

想反悔、回到合并之前：

```bash
git merge --abort
```

如果当时是在做rebase，则用`git rebase --abort`。

> [!warn]标记行必须删干净。只要文件里还留着`<<<<<<<`，代码就会报语法错误，网站也会白屏。提交前全文搜一遍这三种标记。

## 下次怎么避免

开始新任务前先`git pull`；让AI改东西前，先确认你们没有同时在改同一个文件；功能做完尽快合并，别让分支分叉太久。

## 还不行？

冲突很大、看不懂两边的差别时，不要硬选。先`git merge --abort`退回安全状态，再把两边的差异发给AI分析。想整体回到某个好版本，看[undo-ai-changes](/fix/undo-ai-changes)。
