---
title: 包管理器入门：npm 和 yarn 的使用指南
date: 2023-10-05
description: 本课程将深入介绍如何使用 npm 和 yarn 这两个流行的包管理器来管理 JavaScript 项目中的依赖项。
slug: package-managers-npm-yarn
tags:
  - JavaScript
  - npm
  - yarn
category: 前端开发
keywords:
  - 包管理器
  - npm
  - yarn
  - JavaScript 依赖管理
---

# 包管理器（npm 和 yarn）

## 1. 概述

在现代的 JavaScript 开发中，包管理器是不可或缺的工具。它们帮助开发者轻松地安装、管理和更新项目依赖。Node.js 生态系统中最常用的包管理器是 `npm` 和 `yarn`。本教程将详细介绍这两个工具的使用方法，帮助你更好地管理你的 Node.js 项目。

## 2. npm 简介

### 2.1 什么是 npm？

`npm` 是 Node.js 的默认包管理器，全称为 Node Package Manager。它允许开发者从 `npm` 仓库中下载和安装各种第三方库和工具。`npm` 不仅用于安装依赖，还可以用于发布和管理自己的包。

### 2.2 安装 npm

`npm` 通常随 Node.js 一起安装。你可以通过以下命令检查是否已安装 `npm`：

```bash
npm -v
```

如果未安装，可以从 [Node.js 官网](https://nodejs.org/) 下载并安装 Node.js，安装过程中会自动安装 `npm`。

### 2.3 初始化项目

在项目根目录下运行以下命令，初始化一个新的 `npm` 项目：

```bash
npm init
```

运行该命令后，`npm` 会引导你填写项目的基本信息，如项目名称、版本、描述等。完成后，会生成一个 `package.json` 文件，该文件记录了项目的元数据和依赖信息。

### 2.4 安装依赖

使用 `npm install` 命令可以安装项目所需的依赖。例如，安装 `express` 库：

```bash
npm install express
```

安装完成后，`express` 会被添加到 `package.json` 文件的 `dependencies` 字段中，并在 `node_modules` 目录下生成相应的文件。

### 2.5 使用全局安装

有些工具可能需要在全局范围内使用，例如 `nodemon`。你可以使用 `-g` 选项进行全局安装：

```bash
npm install -g nodemon
```

全局安装的包可以在命令行中直接使用。

### 2.6 卸载依赖

如果某个依赖不再需要，可以使用 `npm uninstall` 命令卸载：

```bash
npm uninstall express
```

### 2.7 更新依赖

使用 `npm update` 命令可以更新项目中的依赖：

```bash
npm update
```

### 2.8 使用 `package-lock.json`

`npm` 在安装依赖时会生成一个 `package-lock.json` 文件，该文件记录了每个依赖的确切版本和依赖树。这确保了在不同环境下安装的依赖版本一致。

## 3. yarn 简介

### 3.1 什么是 yarn？

`yarn` 是 Facebook 开发的一个快速、可靠、安全的包管理器。它旨在解决 `npm` 早期版本中的一些性能和安全问题。`yarn` 使用与 `npm` 相同的 `npm` 仓库，因此可以安装相同的包。

### 3.2 安装 yarn

你可以使用 `npm` 安装 `yarn`：

```bash
npm install -g yarn
```

安装完成后，可以通过以下命令检查是否安装成功：

```bash
yarn -v
```

### 3.3 初始化项目

在项目根目录下运行以下命令，初始化一个新的 `yarn` 项目：

```bash
yarn init
```

与 `npm init` 类似，`yarn init` 会生成一个 `package.json` 文件。

### 3.4 安装依赖

使用 `yarn add` 命令可以安装项目所需的依赖。例如，安装 `express` 库：

```bash
yarn add express
```

安装完成后，`express` 会被添加到 `package.json` 文件的 `dependencies` 字段中，并在 `node_modules` 目录下生成相应的文件。

### 3.5 使用全局安装

与 `npm` 类似，`yarn` 也支持全局安装：

```bash
yarn global add nodemon
```

### 3.6 卸载依赖

使用 `yarn remove` 命令可以卸载依赖：

```bash
yarn remove express
```

### 3.7 更新依赖

使用 `yarn upgrade` 命令可以更新项目中的依赖：

```bash
yarn upgrade
```

### 3.8 使用 `yarn.lock`

`yarn` 在安装依赖时会生成一个 `yarn.lock` 文件，该文件记录了每个依赖的确切版本和依赖树。这确保了在不同环境下安装的依赖版本一致。

## 4. npm 与 yarn 的比较

### 4.1 性能

`yarn` 在性能上通常优于 `npm`，尤其是在安装大量依赖时。`yarn` 使用并行下载和缓存机制，使得安装速度更快。

### 4.2 安全性

`yarn` 在安全性方面也有所改进，它通过校验和机制确保下载的包的完整性。

### 4.3 使用场景

- **npm**：适合大多数场景，尤其是当你已经熟悉 `npm` 并且项目规模不大时。
- **yarn**：适合需要高性能和安全性的项目，尤其是大型项目或团队协作项目。

## 5. 实践练习

### 5.1 创建一个简单的 Node.js 项目

1. 创建一个新目录并进入该目录：

   ```bash
   mkdir my-node-app
   cd my-node-app
   ```

2. 初始化项目：

   ```bash
   npm init -y
   ```

   或者

   ```bash
   yarn init -y
   ```

3. 安装 `express` 依赖：

   ```bash
   npm install express
   ```

   或者

   ```bash
   yarn add express
   ```

4. 创建一个 `index.js` 文件，并编写以下代码：

   ```javascript
   const express = require('express');
   const app = express();
   const port = 3000;

   app.get('/', (req, res) => {
     res.send('Hello, World!');
   });

   app.listen(port, () => {
     console.log(`Server is running on http://localhost:${port}`);
   });
   ```

5. 启动服务器：

   ```bash
   node index.js
   ```

6. 打开浏览器访问 `http://localhost:3000`，你应该会看到 `Hello, World!` 的输出。

### 5.2 更新和卸载依赖

1. 更新 `express` 依赖：

   ```bash
   npm update express
   ```

   或者

   ```bash
   yarn upgrade express
   ```

2. 卸载 `express` 依赖：

   ```bash
   npm uninstall express
   ```

   或者

   ```bash
   yarn remove express
   ```

## 6. 总结

在本教程中，我们详细介绍了 `npm` 和 `yarn` 这两个常用的包管理器。通过学习如何初始化项目、安装和卸载依赖、更新依赖等操作，你应该能够更好地管理你的 Node.js 项目。选择合适的包管理器取决于你的项目需求和个人偏好，但无论选择哪个，掌握它们的基本操作都是必不可少的。

希望本教程对你有所帮助，祝你在 Node.js 开发中取得成功！