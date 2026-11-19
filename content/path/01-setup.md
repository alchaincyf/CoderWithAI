---
title: 第1关 · 装好工具
slug: setup
order: 1
level: 1
summary: 装好你那条线的AI工具、Node.js和Git，并让AI回你一句话、新建一个文件。
time: 60到90分钟
cost: 国内线0元；国际线约20美元/月起
goal: 在终端或编辑器里让AI回一句话，并让它在文件夹里新建一个文件
words: [terminal, command, path, cli, nodejs, npm, git, agent, editor]
fixes: [command-not-found, windows-not-recognized, powershell-execution-policy, node-version, xcode-clt-missing, claude-code-region]
updated: 2026-11-18
---

## 这一关你要做成什么

做到这三件事就算过：

1. 电脑上装好了你选的AI工具，并且登录成功。
2. 在一个空文件夹里打开它，它能回你一句话。
3. 你让它新建一个文件，文件夹里真的出现了这个文件。

装的东西一共四样：AI工具、Node.js、Git，以及（国际线）一个有订阅的账号。下面的命令在2026年11月对照官方文档核实过。安装命令会变，如果和你屏幕上看到的不一致，以官方页面为准。

## 先懂一件事：终端是什么

终端是一个只能打字的窗口。你敲一行字，电脑执行，把结果打印回来。AI命令行工具就住在里面。

不熟的话先扫一眼[终端](/words/terminal)和[命令](/words/command)。后面所有「在终端里运行」，意思都是：把我给的那一行原样粘贴进去，按回车。行首如果有`$`符号，不要复制，它只是提示符。

打开终端：

- Mac：按Command+空格，输入「终端」，回车。
- Windows：点开始菜单，搜「PowerShell」，打开。你会看到蓝色或黑色窗口，提示符类似`PS C:\Users\你的名字>`。

先把自己的线选好（没选，回[第0关](/start/what-is-ai-coding)）。下面按「国内线」「国际线」分开写。两条线都要装Node.js和Git，放在最后。

## 动手

### 国内线：TRAE国内版

TRAE是图形界面的AI编辑器，不用先学终端。

#### 第1步：下载并安装

打开浏览器，访问trae.cn。你会看到页面上有一个类似「下载桌面端」的按钮（旁边还有「使用网页端」，先不用）。点下载，按你的系统（Mac或Windows）选对应版本，下载完成后像安装普通软件一样安装：Mac把它拖进「应用程序」，Windows运行安装程序，一路按提示走。

如果看到别的：页面按钮文字可能改版，找「下载」二字即可。不同系统的安装包名称和具体步骤，以官网页面为准。

#### 第2步：登录

第一次打开，它会让你登录，按屏幕上的提示完成就行（登录方式以界面为准）。

你会看到：登录成功后进入主界面，里面能看到文件区域和一个跟AI对话的区域。

如果看到别的：卡在登录页，换个网络或稍后再试，仍不行去官网看帮助。

#### 第3步：确认免费额度

截至2026年11月，官网定价页写着：免费版每月500积分，所有功能可用。积分一般在账户或设置里能查到，找类似「用量」「积分」的入口。

#### 第4步：打开一个空文件夹，跟AI说话

先在桌面（或任何你找得到的位置）新建一个空文件夹，取名`ai-test`。回到TRAE，找类似「打开文件夹」的菜单，选这个文件夹。

TRAE官方文档说，桌面端的IDE窗口有两种模式：「IDE模式」让你对代码改动和执行有细致控制，「SOLO模式」让AI按你一句自然语言需求去规划并完成任务。现在选「IDE模式」就行：看得见文件，更安心。

在AI对话区粘贴：

```prompt 复制给 TRAE
你好。请你做两件事：
1. 用一句中文告诉我你现在是什么模型、当前打开的文件夹叫什么。
2. 在当前文件夹里新建一个叫hello.txt的文件，内容是「今天我装好了AI编程工具」。
做完后告诉我你创建了哪个文件。
```

你会看到：AI回复一句话，然后左边的文件列表里出现`hello.txt`。它可能会问你是否允许创建文件，选同意。

如果看到别的：AI不回，检查是不是还没登录或积分用完。文件没出现，让AI说它把文件建在了哪里。

### 国际线：Claude Code

前提：你人在Anthropic官方支持的地区，并且有Pro、Max等订阅（免费版不含）。大陆读者请回第0关改走国内线。

#### 第1步：安装

Mac，打开终端，粘贴：

```bash
curl -fsSL https://claude.ai/install.sh | bash
```

Windows，打开PowerShell（提示符里有`PS`），粘贴：

```powershell
irm https://claude.ai/install.ps1 | iex
```

Windows如果用的是CMD（提示符没有`PS`），官方给的是另一条命令：

```batch
curl -fsSL https://claude.ai/install.cmd -o install.cmd && install.cmd && del install.cmd
```

你会看到：官方文档特别说明，安装过程不显示进度。所以敲完回车后很久没动静是正常的，等提示符重新出现。

如果看到别的：

- Windows上看到`The token '&&' is not a valid statement separator`，说明你在PowerShell里用了CMD那条命令；看到`'irm' is not recognized`，说明你在CMD里用了PowerShell那条。换对窗口。
- 看到`403`或别的失败，先看[command not found](/fix/command-not-found)，并以官方排错页为准（code.claude.com/docs/en/troubleshoot-install）。

#### 第2步：验证装好

关掉终端，重新打开一个，运行：

```bash
claude --version
```

你会看到：一行版本号，后面带`(Claude Code)`。

如果看到别的：`command not found`或「不是内部或外部命令」，是路径没登记，见[command not found](/fix/command-not-found)（Mac）或[Windows不是内部或外部命令](/fix/windows-not-recognized)。

#### 第3步：登录并对话

先在桌面建一个空文件夹`ai-test`，然后在终端进入它。Mac：

```bash
cd ~/Desktop/ai-test
claude
```

Windows PowerShell：

```powershell
cd $HOME\Desktop\ai-test
claude
```

如果你的桌面被OneDrive接管，路径可能不同，用`cd`到你实际建文件夹的地方即可。

你会看到：第一次运行，它会提示你登录，按官方说法，订阅账号或Console账号在浏览器里完成授权。授权完成回到终端，会出现Claude Code的输入框，上方显示版本、当前模型和工作目录。过程中它还可能问你是否信任这个文件夹，确认是你自己的`ai-test`就选信任。

如果看到别的：浏览器提示「App unavailable in region」之类，是地区问题，见[登录失败、地区不支持](/fix/claude-code-region)。

然后粘贴：

```prompt 复制给 Claude Code
你好。请你做两件事：
1. 用一句中文告诉我你现在是什么模型、当前所在的文件夹叫什么。
2. 在当前文件夹里新建一个叫hello.txt的文件，内容是「今天我装好了AI编程工具」。
做完后告诉我你创建了哪个文件。
```

你会看到：它先回答，然后要新建文件时停下来问你，类似选项里有「Yes」。选Yes批准。随后文件夹里出现`hello.txt`。

退出：输入`/exit`，或按两次Ctrl+D。

### 国际线：Codex

前提同上，用ChatGPT账号登录。OpenAI官方的[受支持国家和地区名单](https://developers.openai.com/api/docs/supported-countries)不包含中国大陆，名单之外使用可能被封号或停用，和Claude Code一样不适合大陆直接使用，本站不教绕过的办法。

安装命令（来自OpenAI官方仓库的说明）：

Mac，终端：

```bash
curl -fsSL https://chatgpt.com/codex/install.sh | sh
```

Windows，PowerShell：

```powershell
powershell -ExecutionPolicy ByPass -c "irm https://chatgpt.com/codex/install.ps1 | iex"
```

也可以在装好Node.js后用npm：`npm install -g @openai/codex`，或在Mac用Homebrew：`brew install --cask codex`。

进入`ai-test`文件夹后运行：

```bash
codex
```

你会看到：首次运行让你选登录方式，选「Sign in with ChatGPT」，在浏览器里授权。成功后终端里出现一个横幅，显示模型、当前目录和几个斜杠命令（如`/init`、`/status`），然后是输入提示。粘贴上面那段「你好……」的提示词，它会在建文件前征求你的许可。

如果看到别的：PowerShell报「禁止运行脚本」，见[PowerShell执行策略](/fix/powershell-execution-policy)。

### 可选：国内线的命令行，Kimi Code

想在终端里用国产模型，可以装Kimi Code。以下命令来自其官方资料整理，请以kimi.com/code页面为准。Mac：

```bash
curl -fsSL https://code.kimi.com/kimi-code/install.sh | bash
```

Windows PowerShell：

```powershell
irm https://code.kimi.com/kimi-code/install.ps1 | iex
```

装完运行`kimi`，再输入`/login`登录。使用需要Kimi会员，价格以官网为准。

### 两条线都要装：Node.js

很多AI生成的项目、后面第3关以及更多关卡，会用到Node.js。第2关的小网页不需要它，但现在装一次，后面不用回头。

打开nodejs.org，下载标有LTS（长期支持）的版本，按你的系统安装，一路按提示走。不要选带「Current」字样的。

装完关掉终端重开，运行：

```bash
node -v
```

你会看到：一个以v开头的版本号。如果提示找不到命令，见[command not found](/fix/command-not-found)；提示版本太旧，见[Node版本太旧](/fix/node-version)。

### 两条线都要装：Git

Git用来存档你的每一次修改，第2关会用到。

Mac：在终端运行：

```bash
git --version
```

如果没装过，系统通常会弹出一个窗口，提示安装命令行开发者工具，点安装、等它装完。完成后再运行一次，应显示版本号。没弹出或报错，见[Xcode命令行工具缺失](/fix/xcode-clt-missing)。

Windows：（国际线的Claude Code官方文档建议装Git for Windows，这样它能用Bash工具，不装也能用。）访问git-scm.com，进入Windows的下载页，下载安装程序运行。安装过程会连续问你很多选项，不确定就保持默认，一路下一步。装完重新打开PowerShell，运行`git --version`。

你会看到：一行类似`git version ...`的输出。

## 验收：怎么知道真的成了

- 在`ai-test`文件夹里能找到AI创建的`hello.txt`，内容是我指定的那句话。
- 终端运行`node -v`和`git --version`都有版本号输出。
- 国际线：`claude --version`或`codex`能正常启动。
- 国内线：TRAE能登录，并在IDE模式下完成了同样的对话。

## 最常卡在这几个地方

1. 装完提示找不到命令：多数是没新开终端，或路径没登记。看[command not found](/fix/command-not-found)。
2. Windows报「不是内部或外部命令」：见[Windows not recognized](/fix/windows-not-recognized)。
3. PowerShell禁止运行脚本：见[执行策略](/fix/powershell-execution-policy)。
4. Node版本太旧：见[Node版本](/fix/node-version)。
5. 国际线登录失败：先看是不是地区问题，见[claude-code-region](/fix/claude-code-region)。

## 这一关的提示词合集

```prompt
你好。请你做两件事：
1. 用一句中文告诉我你现在是什么模型、当前所在的文件夹叫什么。
2. 在当前文件夹里新建一个叫hello.txt的文件，内容是「今天我装好了AI编程工具」。
做完后告诉我你创建了哪个文件。
```
