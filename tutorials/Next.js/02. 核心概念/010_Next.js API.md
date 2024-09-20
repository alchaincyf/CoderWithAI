---
title: Next.js API 路由详解 (App Router)
date: 2023-10-05
description: 本课程详细介绍如何在Next.js的App Router中创建和使用API路由，包括路由配置、请求处理和响应管理。
slug: nextjs-api-routes-app-router
tags:
  - Next.js
  - API路由
  - 后端开发
  - App Router
category: 编程教程
keywords:
  - Next.js API
  - API路由
  - Next.js后端
  - App Router
---
# API 路由 (App Router)

在现代Web开发中，API路由是构建动态应用的关键部分。Next.js 提供了一种简单而强大的方式来创建和处理API路由，使得开发者可以在同一个项目中同时处理前端和后端逻辑。本教程将详细介绍如何在Next.js的App Router中创建和使用API路由。

## 1. 什么是API路由？

API路由是Next.js中的一种特殊路由，允许你在应用中创建后端API。这些API路由可以处理HTTP请求，并返回JSON格式的数据。通过API路由，你可以在同一个Next.js项目中同时处理前端和后端逻辑，而不需要单独设置一个服务器。

API路由的主要优势包括：
- **简化开发流程**：前后端代码在同一项目中，便于管理和部署
- **无需额外配置**：Next.js自动处理路由映射
- **支持服务器端渲染 (SSR)**：可以轻松实现SSR和静态生成
- **安全性**：API路由运行在服务器端，可以安全地处理敏感信息

## 2. 创建第一个API路由 (App Router)

在Next.js的App Router中，API路由位于`app/api`目录下。每个文件都会自动映射到一个API端点。

### 2.1 创建API文件

首先，在`app/api`目录下创建一个新的文件`hello/route.js`。这个文件将定义我们的API路由。

在`hello/route.js`中，我们可以添加以下代码来创建一个简单的GET请求处理程序：

```javascript
export async function GET(request) {
  return new Response(JSON.stringify({ message: 'Hello, Next.js API!' }), {
    headers: { 'Content-Type': 'application/json' },
  });
}

### 2.2 访问API路由

当你运行开发服务器后，可以通过访问 `http://localhost:3000/api/hello` 来查看API的返回结果。你应该会看到如下JSON响应：

```json
json
复制代码
{
  "message": "Hello, Next.js API!"
}

```

### 2.3 请求方法支持

除了GET请求，API路由也支持其他HTTP方法，例如POST、PUT、DELETE等。你可以通过在同一个`route.js`文件中定义多个函数来处理不同的请求方法。例如，以下是一个处理POST请求的示例：

```jsx
javascript
复制代码
export async function POST(request) {
  const body = await request.json();
  return new Response(JSON.stringify({ message: `Hello, ${body.name}!` }), {
    headers: { 'Content-Type': 'application/json' },
  });
}

```

你可以通过发送POST请求并在请求体中包含一个`name`字段，来接收动态的响应。

## 3. 中间件与错误处理

在处理API请求时，有时需要执行一些中间处理逻辑，例如身份验证或日志记录。你可以在API路由中添加自定义的中间件逻辑。例如，检查请求是否包含授权头：

```jsx
javascript
复制代码
export async function GET(request) {
  const authHeader = request.headers.get('Authorization');

  if (!authHeader || authHeader !== 'Bearer my-secret-token') {
    return new Response(JSON.stringify({ error: 'Unauthorized' }), {
      status: 401,
      headers: { 'Content-Type': 'application/json' },
    });
  }

  return new Response(JSON.stringify({ message: 'Authorized request' }), {
    headers: { 'Content-Type': 'application/json' },
  });
}

```

这样，如果请求不包含正确的授权令牌，服务器将返回401未授权错误。

## 4. 部署API路由

在Next.js中，API路由与页面路由一样，部署过程是一样的。你只需将项目部署到支持Next.js的平台（如Vercel），API路由将自动成为你部署的API服务的一部分，无需额外配置。

## 5. 小结

Next.js的API路由功能使得前后端的开发工作更加整合和简化。你可以在一个项目中同时处理用户界面和后端API，并利用Next.js的功能如服务器端渲染和静态生成，构建强大且灵活的Web应用。通过本文介绍的步骤，你可以轻松上手API路由并根据需求扩展功能。