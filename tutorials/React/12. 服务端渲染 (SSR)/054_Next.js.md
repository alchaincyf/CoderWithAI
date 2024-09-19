---
title: Next.js 基础教程
date: 2023-10-05
description: 本课程将带你从零开始学习Next.js，掌握其核心概念和基本用法，包括页面路由、数据获取、静态生成和服务器端渲染等。
slug: nextjs-basic-tutorial
tags:
  - Next.js
  - React
  - 前端开发
category: 前端开发
keywords:
  - Next.js基础
  - React框架
  - 前端教程
---

# Next.js 基础教程

## 1. 概述

Next.js 是一个基于 React 的框架，旨在简化 React 应用的开发。它提供了许多开箱即用的功能，如服务器端渲染（SSR）、静态站点生成（SSG）、路由、API 路由等。本教程将带你从零开始学习 Next.js 的基础知识。

## 2. 环境搭建

### 2.1 Node.js 和 npm 安装

首先，确保你已经安装了 Node.js 和 npm。你可以通过以下命令检查是否已安装：

```bash
node -v
npm -v
```

如果没有安装，请访问 [Node.js 官网](https://nodejs.org/) 下载并安装。

### 2.2 创建 Next.js 项目

使用 `create-next-app` 命令可以快速创建一个新的 Next.js 项目：

```bash
npx create-next-app my-next-app
cd my-next-app
npm run dev
```

现在，打开浏览器并访问 `http://localhost:3000`，你应该会看到一个默认的 Next.js 页面。

## 3. 页面和路由

### 3.1 页面基础

在 Next.js 中，每个 `.js`、`.jsx`、`.ts` 或 `.tsx` 文件都会自动成为路由。例如，`pages/index.js` 对应根路径 `/`，`pages/about.js` 对应 `/about`。

```javascript
// pages/index.js
export default function Home() {
  return <div>Welcome to Next.js!</div>;
}

// pages/about.js
export default function About() {
  return <div>About Page</div>;
}
```

### 3.2 动态路由

Next.js 支持动态路由，通过在文件名中使用方括号 `[]` 来定义动态参数。例如，`pages/posts/[id].js` 可以匹配 `/posts/1`、`/posts/2` 等。

```javascript
// pages/posts/[id].js
import { useRouter } from 'next/router';

export default function Post() {
  const router = useRouter();
  const { id } = router.query;

  return <div>Post ID: {id}</div>;
}
```

## 4. 数据获取

### 4.1 服务器端渲染 (SSR)

使用 `getServerSideProps` 函数可以在服务器端获取数据并将其传递给页面组件。

```javascript
// pages/posts/[id].js
export async function getServerSideProps(context) {
  const { id } = context.params;
  const res = await fetch(`https://api.example.com/posts/${id}`);
  const post = await res.json();

  return {
    props: { post },
  };
}

export default function Post({ post }) {
  return (
    <div>
      <h1>{post.title}</h1>
      <p>{post.content}</p>
    </div>
  );
}
```

### 4.2 静态生成 (SSG)

使用 `getStaticProps` 和 `getStaticPaths` 可以生成静态页面。

```javascript
// pages/posts/[id].js
export async function getStaticPaths() {
  const res = await fetch('https://api.example.com/posts');
  const posts = await res.json();

  const paths = posts.map((post) => ({
    params: { id: post.id.toString() },
  }));

  return { paths, fallback: false };
}

export async function getStaticProps({ params }) {
  const res = await fetch(`https://api.example.com/posts/${params.id}`);
  const post = await res.json();

  return { props: { post } };
}

export default function Post({ post }) {
  return (
    <div>
      <h1>{post.title}</h1>
      <p>{post.content}</p>
    </div>
  );
}
```

## 5. API 路由

Next.js 允许你在 `pages/api` 目录下创建 API 路由。这些路由可以处理后端逻辑，类似于 Express.js。

```javascript
// pages/api/hello.js
export default function handler(req, res) {
  res.status(200).json({ message: 'Hello, Next.js!' });
}
```

## 6. 实践练习

### 6.1 创建一个简单的博客应用

1. 创建一个 `pages/index.js` 页面，显示博客文章列表。
2. 创建一个 `pages/posts/[id].js` 页面，显示单篇文章的详细内容。
3. 创建一个 `pages/api/posts.js` API 路由，返回博客文章数据。

### 6.2 实现动态路由和数据获取

1. 使用 `getStaticPaths` 和 `getStaticProps` 实现静态生成。
2. 使用 `getServerSideProps` 实现服务器端渲染。

## 7. 总结

通过本教程，你已经学习了 Next.js 的基础知识，包括环境搭建、页面和路由、数据获取以及 API 路由。Next.js 提供了强大的功能，帮助你快速构建高性能的 React 应用。继续探索 Next.js 的更多高级功能，如 SEO 优化、性能分析工具等，将进一步提升你的开发技能。

---

希望这篇教程对你有所帮助！如果你有任何问题或需要进一步的指导，请随时提问。