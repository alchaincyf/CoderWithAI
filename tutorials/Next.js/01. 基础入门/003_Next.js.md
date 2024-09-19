---
title: 创建你的第一个Next.js应用
date: 2023-10-05
description: 本教程将引导你从零开始创建你的第一个Next.js应用，涵盖项目设置、页面创建、路由配置等基础内容。
slug: create-first-nextjs-app
tags:
  - Next.js
  - 前端开发
  - React
category: 编程教程
keywords:
  - Next.js入门
  - 创建Next.js应用
  - Next.js教程
---

# 创建第一个Next.js应用

在本教程中，我们将带领你创建你的第一个Next.js应用。无论你是前端开发的新手，还是有一定经验的开发者，本教程都将为你提供一个清晰、易于理解的指南。我们将从环境搭建开始，逐步引导你完成应用的创建，并解释每个步骤背后的理论。

## 1. Next.js简介

Next.js是一个基于React的框架，旨在简化React应用的开发。它提供了许多开箱即用的功能，如服务器端渲染（SSR）、静态站点生成（SSG）、自动代码分割、路由系统等。Next.js的目标是让开发者能够快速构建高性能、可扩展的Web应用。

## 2. 环境搭建

在开始创建Next.js应用之前，我们需要确保你的开发环境已经准备就绪。

### 2.1 安装Node.js

Next.js需要Node.js环境。你可以通过以下命令检查是否已经安装了Node.js：

```bash
node -v
```

如果没有安装，请访问[Node.js官网](https://nodejs.org/)下载并安装最新版本的Node.js。

### 2.2 安装npm或yarn

npm是Node.js的包管理工具，通常随Node.js一起安装。你也可以选择使用yarn作为替代。你可以通过以下命令检查是否已经安装了npm或yarn：

```bash
npm -v
# 或
yarn -v
```

如果没有安装yarn，你可以通过以下命令安装：

```bash
npm install -g yarn
```

## 3. 创建第一个Next.js应用

现在，我们已经准备好了开发环境，接下来我们将创建第一个Next.js应用。

### 3.1 使用create-next-app创建项目

Next.js提供了一个名为`create-next-app`的CLI工具，可以帮助我们快速创建一个新的Next.js项目。你可以通过以下命令创建一个新的项目：

```bash
npx create-next-app my-first-nextjs-app
```

或者使用yarn：

```bash
yarn create next-app my-first-nextjs-app
```

这里的`my-first-nextjs-app`是你的项目名称，你可以根据需要更改。

### 3.2 项目结构

创建完成后，进入项目目录：

```bash
cd my-first-nextjs-app
```

项目结构如下：

```
my-first-nextjs-app/
├── node_modules/
├── public/
├── styles/
├── .gitignore
├── package.json
├── README.md
└── next.config.js
```

- `node_modules/`：包含项目依赖的模块。
- `public/`：存放静态资源，如图片、字体等。
- `styles/`：存放CSS样式文件。
- `.gitignore`：指定Git忽略的文件和目录。
- `package.json`：项目的配置文件，包含依赖项和脚本。
- `README.md`：项目的说明文档。
- `next.config.js`：Next.js的配置文件。

### 3.3 启动开发服务器

在项目目录下，运行以下命令启动开发服务器：

```bash
npm run dev
# 或
yarn dev
```

默认情况下，开发服务器会在`http://localhost:3000`上运行。打开浏览器，访问该地址，你应该会看到一个欢迎页面。

## 4. 创建第一个页面

Next.js使用文件系统作为路由系统。每个在`pages/`目录下的文件都会自动映射到一个路由。

### 4.1 创建一个新页面

在`pages/`目录下创建一个名为`about.js`的文件，内容如下：

```javascript
// pages/about.js
import React from 'react';

const About = () => {
  return (
    <div>
      <h1>关于我们</h1>
      <p>这是一个关于我们的页面。</p>
    </div>
  );
};

export default About;
```

### 4.2 访问新页面

保存文件后，访问`http://localhost:3000/about`，你应该会看到刚刚创建的“关于我们”页面。

## 5. 添加样式

Next.js支持多种CSS解决方案，包括CSS Modules、Sass、Styled-components等。

### 5.1 使用CSS Modules

在`styles/`目录下创建一个名为`About.module.css`的文件，内容如下：

```css
/* styles/About.module.css */
.container {
  padding: 20px;
  background-color: #f0f0f0;
  border-radius: 8px;
}

.title {
  font-size: 24px;
  color: #333;
}

.description {
  font-size: 16px;
  color: #666;
}
```

然后在`pages/about.js`中引入并使用这些样式：

```javascript
// pages/about.js
import React from 'react';
import styles from '../styles/About.module.css';

const About = () => {
  return (
    <div className={styles.container}>
      <h1 className={styles.title}>关于我们</h1>
      <p className={styles.description}>这是一个关于我们的页面。</p>
    </div>
  );
};

export default About;
```

保存文件后，刷新页面，你应该会看到样式已经应用到页面上了。

## 6. 实践练习

现在，你已经掌握了创建Next.js应用的基本步骤。接下来，尝试以下练习：

1. 创建一个新的页面，如“联系我们”页面。
2. 为新页面添加样式，使用CSS Modules或其他CSS解决方案。
3. 在主页（`pages/index.js`）中添加一个导航栏，链接到“关于我们”和“联系我们”页面。

## 7. 总结

在本教程中，我们学习了如何创建第一个Next.js应用。我们从环境搭建开始，逐步完成了项目的创建、页面的添加以及样式的应用。通过这些步骤，你应该对Next.js的基本概念和使用方法有了初步的了解。

在接下来的课程中，我们将深入探讨Next.js的更多高级功能，如页面和路由、服务端渲染、静态站点生成、数据获取方法等。希望你能继续保持学习的热情，探索Next.js的更多可能性！