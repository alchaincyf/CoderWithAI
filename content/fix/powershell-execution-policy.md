---
title: "PowerShell提示「在此系统上禁止运行脚本」怎么办"
slug: powershell-execution-policy
summary: Windows默认拦截脚本运行，只给当前用户放行本地脚本即可。
category: 安装与环境
match: ["在此系统上禁止运行脚本", "running scripts is disabled on this system", "about_Execution_Policies", "UnauthorizedAccess", "npm.ps1", "PSSecurityException"]
platform: [windows]
updated: 2026-11-18
---

```error
npm : 无法加载文件 C:\Users\你的用户名\AppData\Roaming\npm\npm.ps1，因为在此系统上禁止运行脚本。有关详细信息，请参阅 https:/go.microsoft.com/fwlink/?LinkID=135170 中的 about_Execution_Policies。
    + CategoryInfo          : SecurityError: (:) []，PSSecurityException
    + FullyQualifiedErrorId : UnauthorizedAccess
```

英文系统里是`running scripts is disabled on this system`。

## 人话：这是怎么回事

Windows的PowerShell有一道「执行策略」安全锁，默认很严，连npm这类工具自带的脚本也会拦住。锁住的是`.ps1`脚本，不是你的电脑坏了。

微软文档说明：如果所有范围都没设置，Windows客户端的有效策略是Restricted，也就是不允许运行任何脚本。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我在Windows PowerShell里运行npm报「因为在此系统上禁止运行脚本」。
请先用`Get-ExecutionPolicy -List`看当前策略并向我解释。
修复只允许：`Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser`。
不要改LocalMachine，不要设成Unrestricted或Bypass，不要关闭任何安全功能。
改完让我重开PowerShell，运行`npm -v`验证能输出版本号。
```

## 想自己动手的话

先看现状：

```powershell
Get-ExecutionPolicy -List
```

这条命令列出各个范围的策略。

然后只对你自己的账户放行本地脚本（微软文档里的标准写法）：

```powershell
Set-ExecutionPolicy -ExecutionPolicy RemoteSigned -Scope CurrentUser
```

RemoteSigned的意思：自己电脑上写的脚本能跑，从网上下载的未签名脚本不能跑。询问时输入Y回车。

只想这一个窗口临时放行，关窗口就恢复：

```powershell
Set-ExecutionPolicy -Scope Process -ExecutionPolicy Bypass
```

这条命令只在当前窗口生效，关掉窗口就恢复，和上面提示词里「不要设成Bypass」针对的永久设置并不矛盾。不想改任何设置的话，改用CMD（黑底的命令提示符）运行同样的命令，CMD不受这个限制。

> [!warn]网上常见的建议是以管理员身份把策略设成Unrestricted或对整台电脑Bypass。这会让所有用户都能跑任何脚本，范围比你需要的大得多。CurrentUser加RemoteSigned就够用。

## 下次怎么避免

新电脑装完Node.js后先执行一次上面的RemoteSigned命令。

## 还不行？

- 提示被「组策略」控制、改不了：这是公司电脑的管理策略，找IT管理员。
- 提示npm不是内部或外部命令：看[Windows找不到命令](/fix/windows-not-recognized)。
