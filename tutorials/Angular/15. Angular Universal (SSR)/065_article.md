---
title: 服务端渲染基础教程
date: 2023-10-05
description: 本课程将深入探讨服务端渲染（SSR）的基础知识，包括其工作原理、优势以及如何在现代Web应用中实现。
slug: server-side-rendering-basics
tags:
  - 服务端渲染
  - SSR
  - Web开发
category: 前端开发
keywords:
  - 服务端渲染
  - SSR
  - 前端开发
---

# 服务端渲染基础

## 引言

服务端渲染（Server-Side Rendering，简称 SSR）是一种在服务器端生成完整 HTML 页面的技术，然后将这些页面发送给客户端。与客户端渲染（Client-Side Rendering，简称 CSR）相比，SSR 可以提高应用的首次加载速度，改善 SEO（搜索引擎优化），并提供更好的用户体验。

在本教程中，我们将深入探讨 Angular 中的服务端渲染基础，包括其工作原理、实现步骤以及相关的最佳实践。

## 1. 服务端渲染的优势

### 1.1 提高首次加载速度

在 CSR 中，浏览器需要下载 JavaScript 文件并执行它们，才能渲染页面。而 SSR 在服务器端生成 HTML 页面，客户端只需下载并显示这些页面，从而减少了首次加载时间。

### 1.2 改善 SEO

搜索引擎爬虫通常不执行 JavaScript，因此 CSR 应用可能无法正确索引。SSR 生成的 HTML 页面可以直接被爬虫解析，从而提高 SEO 效果。

### 1.3 更好的用户体验

SSR 可以提供更快的页面加载速度和更好的可访问性，尤其是在网络条件较差或设备性能较低的情况下。

## 2. Angular 中的服务端渲染

Angular 通过 Angular Universal 提供了对 SSR 的支持。Angular Universal 是一个库，允许你在服务器端运行 Angular 应用，并生成静态 HTML 页面。

### 2.1 安装 Angular Universal

首先，我们需要安装 Angular Universal。你可以通过 Angular CLI 来完成这一步骤。

```bash
ng add @nguniversal/express-engine
```

这个命令会自动安装所需的依赖项，并生成必要的文件和配置。

### 2.2 项目结构

安装完成后，你的项目结构将发生变化。主要的文件和目录包括：

- `server.ts`: 服务器端入口文件，负责启动 Express 服务器并渲染 Angular 应用。
- `main.server.ts`: 服务器端应用的入口文件。
- `app.server.module.ts`: 服务器端应用的模块文件。

### 2.3 配置服务器

`server.ts` 文件包含了 Express 服务器的配置。你可以在这里定义路由、处理请求等。

```typescript
import 'zone.js/dist/zone-node';
import { ngExpressEngine } from '@nguniversal/express-engine';
import * as express from 'express';
import { join } from 'path';

import { AppServerModule } from './src/main.server';
import { APP_BASE_HREF } from '@angular/common';
import { existsSync } from 'fs';

// Express server
const app = express();

const DIST_FOLDER = join(process.cwd(), 'dist/browser');

// Our Universal express-engine (found @ https://github.com/angular/universal/tree/master/modules/express-engine)
app.engine('html', ngExpressEngine({
  bootstrap: AppServerModule,
}));

app.set('view engine', 'html');
app.set('views', DIST_FOLDER);

// Example Express Rest API endpoints
// app.get('/api/**', (req, res) => { });
// Serve static files from /browser
app.get('*.*', express.static(DIST_FOLDER, {
  maxAge: '1y'
}));

// All regular routes use the Universal engine
app.get('*', (req, res) => {
  res.render('index', { req, providers: [{ provide: APP_BASE_HREF, useValue: req.baseUrl }] });
});

// Start up the Node server
app.listen(process.env.PORT || 4000, () => {
  console.log(`Node Express server listening on http://localhost:${process.env.PORT || 4000}`);
});
```

### 2.4 运行服务器

配置完成后，你可以通过以下命令启动服务器：

```bash
npm run build:ssr && npm run serve:ssr
```

这将构建你的 Angular 应用，并在服务器端运行它。

## 3. 实践练习

### 3.1 创建一个简单的 Angular 应用

首先，创建一个简单的 Angular 应用，包含一个组件和一个服务。

```bash
ng new ssr-demo
cd ssr-demo
ng generate component home
ng generate service data
```

### 3.2 实现服务端渲染

按照前面的步骤，安装 Angular Universal 并配置服务器。

### 3.3 测试应用

启动服务器并访问应用，检查页面是否在服务器端正确渲染。

```bash
npm run build:ssr && npm run serve:ssr
```

打开浏览器，访问 `http://localhost:4000`，你应该能看到你的 Angular 应用在服务器端渲染的页面。

## 4. 常见问题与解决方案

### 4.1 数据获取

在 SSR 中，数据获取是一个常见的问题。由于服务器端没有浏览器环境，你不能直接使用 `window` 或 `document` 对象。你可以使用 Angular 的 `TransferState` 机制来解决这个问题。

### 4.2 样式和脚本

在 SSR 中，样式和脚本的加载也需要特别处理。你可以使用 Angular 的 `DomRenderer` 和 `ServerTransferStateModule` 来处理这些问题。

## 5. 总结

服务端渲染是提高应用性能和 SEO 的有效手段。通过 Angular Universal，你可以轻松地将现有的 Angular 应用转换为支持 SSR 的应用。希望本教程能帮助你理解 SSR 的基本概念和实现方法，并在实际项目中应用这些知识。

## 6. 进一步学习

- [Angular Universal 官方文档](https://angular.io/guide/universal)
- [Express 官方文档](https://expressjs.com/)
- [Angular 数据获取与状态管理](https://angular.io/guide/http)

通过这些资源，你可以进一步深入学习 Angular 中的服务端渲染技术，并将其应用到更复杂的项目中。