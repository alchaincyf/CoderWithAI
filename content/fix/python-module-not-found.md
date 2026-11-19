---
title: "ModuleNotFoundError: No module named 'xxx'怎么办"
slug: python-module-not-found
summary: Python找不到某个包。没装，或装到了另一个Python里。
category: 安装与环境
match: ["ModuleNotFoundError: No module named", "ModuleNotFoundError", "No module named", "ImportError: No module named"]
platform: [all]
updated: 2026-11-18
---

```error
Traceback (most recent call last):
  File "main.py", line 1, in <module>
    import requests
ModuleNotFoundError: No module named 'requests'
```

## 人话：这是怎么回事

代码里写了`import requests`（要用requests这个包），[Python](/words/python)去找，没找到。两种原因：这个包确实没装；或者你装过，但装到了另一个Python里（电脑上可能有好几个Python），运行代码的这个看不到。

很常见，尤其是AI生成了脚本却没提醒你先装依赖。

## 最快的解法：把这段发给AI

```prompt 复制给 Claude Code / Codex
我运行Python脚本时报ModuleNotFoundError: No module named 'xxx'（报错原文见下）：

（在这里粘贴报错）

请先运行`which python3`和`python3 -m pip --version`，确认运行代码的Python和pip是不是同一个。
在项目里创建并启用虚拟环境`.venv`，用`python3 -m pip install`装好缺的包，并更新requirements.txt。
注意区分包名和导入名（比如import cv2对应opencv-python）。
不要使用`--break-system-packages`或`sudo pip`。最后重新运行脚本，确认通过。
```

## 想自己动手的话

关键习惯：用`python3 -m pip`代替直接写`pip`，这样一定装到你运行的那个Python里。

```bash
python3 -m pip install requests
```

如果这一步报externally-managed-environment，看[pip受保护](/fix/pip-externally-managed)，先建虚拟环境。

注意，有些包的安装名和代码里import的名字不一样：

- `import cv2`，安装`opencv-python`
- `import PIL`，安装`pillow`
- `import yaml`，安装`pyyaml`
- `import sklearn`，安装`scikit-learn`
- `import bs4`，安装`beautifulsoup4`

验证装好了：

```bash
python3 -c "import requests; print(requests.__version__)"
```

打印出版本号就成功了。Windows上把`python3`换成`python`或`py`。

## 下次怎么避免

每个项目有自己的虚拟环境和requirements.txt，让AI每次新增依赖都同步更新。

## 还不行？

- 装包被系统拒绝：看[externally-managed-environment](/fix/pip-externally-managed)。
- 终端找不到python3：看[command not found](/fix/command-not-found)。
- 网络超时：看[终端不走代理](/fix/terminal-proxy)。
