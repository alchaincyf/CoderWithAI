---
title: 开发环境搭建：Vue CLI 与 Vite 教程
date: 2023-10-05
description: 本课程详细讲解如何使用 Vue CLI 和 Vite 搭建高效的 Vue.js 开发环境，涵盖安装、配置和项目初始化。
slug: vue-cli-vite-setup
tags:
  - Vue.js
  - 开发环境
  - Vite
category: 前端开发
keywords:
  - Vue CLI
  - Vite
  - 开发环境搭建
---

# 开发环境搭建 (Vue CLI, Vite)

## 1. 概述

在开始使用 Vue.js 进行开发之前，首先需要搭建一个合适的开发环境。Vue.js 提供了两种主要的工具来帮助我们快速搭建项目：Vue CLI 和 Vite。Vue CLI 是一个功能丰富的脚手架工具，而 Vite 是一个更轻量级、更快速的构建工具。本教程将详细介绍如何使用这两种工具来搭建 Vue.js 开发环境。

## 2. Vue CLI 简介

### 2.1 什么是 Vue CLI？

Vue CLI 是一个官方提供的脚手架工具，用于快速搭建 Vue.js 项目。它集成了许多常用的开发工具和配置，使得开发者可以专注于业务逻辑的开发，而不必花费大量时间在项目配置上。

### 2.2 安装 Vue CLI

首先，你需要确保你的电脑上已经安装了 Node.js 和 npm（Node.js 的包管理器）。你可以通过以下命令来检查它们是否已经安装：

```bash
node -v
npm -v
```

如果还没有安装，你可以从 [Node.js 官网](https://nodejs.org/) 下载并安装。

安装 Vue CLI 非常简单，只需在终端中运行以下命令：

```bash
npm install -g @vue/cli
```

安装完成后，你可以通过以下命令来验证安装是否成功：

```bash
vue --version
```

### 2.3 创建 Vue CLI 项目

使用 Vue CLI 创建一个新的 Vue.js 项目非常简单。只需在终端中运行以下命令：

```bash
vue create my-vue-app
```

`my-vue-app` 是你项目的名称，你可以根据需要进行修改。运行命令后，Vue CLI 会提示你选择一些配置选项，例如选择预设配置、是否使用 TypeScript 等。你可以根据自己的需求进行选择。

### 2.4 启动开发服务器

项目创建完成后，进入项目目录并启动开发服务器：

```bash
cd my-vue-app
npm run serve
```

启动成功后，你可以在浏览器中访问 `http://localhost:8080` 来查看你的 Vue.js 应用。

## 3. Vite 简介

### 3.1 什么是 Vite？

Vite 是一个由 Vue.js 作者尤雨溪开发的构建工具，旨在提供更快的开发体验。Vite 利用了现代浏览器对 ES 模块的原生支持，使得开发服务器启动和热更新速度非常快。

### 3.2 安装 Vite

Vite 的安装也非常简单。你可以通过以下命令来全局安装 Vite：

```bash
npm install -g create-vite-app
```

### 3.3 创建 Vite 项目

使用 Vite 创建一个新的 Vue.js 项目同样非常简单。只需在终端中运行以下命令：

```bash
npm init vite-app my-vite-app
```

`my-vite-app` 是你项目的名称，你可以根据需要进行修改。

### 3.4 启动开发服务器

项目创建完成后，进入项目目录并启动开发服务器：

```bash
cd my-vite-app
npm install
npm run dev
```

启动成功后，你可以在浏览器中访问 `http://localhost:3000` 来查看你的 Vue.js 应用。

## 4. 对比 Vue CLI 和 Vite

### 4.1 性能对比

- **Vue CLI**：Vue CLI 使用 Webpack 作为构建工具，虽然功能强大，但在项目规模较大时，启动和热更新速度可能会较慢。
- **Vite**：Vite 利用了现代浏览器对 ES 模块的原生支持，启动和热更新速度非常快，尤其适合大型项目。

### 4.2 适用场景

- **Vue CLI**：适合需要复杂配置和插件的项目，或者需要兼容旧版浏览器的项目。
- **Vite**：适合追求快速开发体验的项目，尤其是现代浏览器环境下的项目。

## 5. 实践练习

### 5.1 使用 Vue CLI 创建一个简单的计数器应用

1. 使用 Vue CLI 创建一个新的项目：

   ```bash
   vue create counter-app
   ```

2. 进入项目目录并启动开发服务器：

   ```bash
   cd counter-app
   npm run serve
   ```

3. 打开 `src/App.vue` 文件，修改内容如下：

   ```vue
   <template>
     <div id="app">
       <h1>{{ count }}</h1>
       <button @click="increment">Increment</button>
     </div>
   </template>

   <script>
   export default {
     data() {
       return {
         count: 0,
       };
     },
     methods: {
       increment() {
         this.count++;
       },
     },
   };
   </script>

   <style>
   #app {
     text-align: center;
     margin-top: 60px;
   }
   </style>
   ```

4. 在浏览器中查看应用，点击按钮可以看到计数器的值增加。

### 5.2 使用 Vite 创建一个简单的计数器应用

1. 使用 Vite 创建一个新的项目：

   ```bash
   npm init vite-app counter-app-vite
   ```

2. 进入项目目录并启动开发服务器：

   ```bash
   cd counter-app-vite
   npm install
   npm run dev
   ```

3. 打开 `src/App.vue` 文件，修改内容如下：

   ```vue
   <template>
     <div id="app">
       <h1>{{ count }}</h1>
       <button @click="increment">Increment</button>
     </div>
   </template>

   <script>
   export default {
     data() {
       return {
         count: 0,
       };
     },
     methods: {
       increment() {
         this.count++;
       },
     },
   };
   </script>

   <style>
   #app {
     text-align: center;
     margin-top: 60px;
   }
   </style>
   ```

4. 在浏览器中查看应用，点击按钮可以看到计数器的值增加。

## 6. 总结

通过本教程，你已经学会了如何使用 Vue CLI 和 Vite 来搭建 Vue.js 开发环境。Vue CLI 提供了丰富的功能和配置选项，适合需要复杂配置的项目；而 Vite 则提供了更快的开发体验，适合追求效率的项目。根据你的项目需求，选择合适的工具来搭建你的开发环境吧！

在接下来的课程中，我们将深入学习 Vue.js 的各个方面，包括组件化开发、状态管理、路由配置等。继续加油！