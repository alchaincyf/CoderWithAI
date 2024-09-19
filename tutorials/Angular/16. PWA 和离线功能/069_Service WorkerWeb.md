---
title: 深入理解Service Worker：构建离线Web应用
date: 2023-10-05
description: 本课程将深入探讨Service Worker的工作原理，教你如何使用它来构建离线可用的Web应用，提升用户体验。
slug: service-worker-offline-web-apps
tags:
  - Service Worker
  - Web开发
  - 离线应用
category: 前端开发
keywords:
  - Service Worker
  - 离线应用
  - Web应用
---

# Service Worker 教程

## 1. 什么是 Service Worker？

Service Worker 是一种在浏览器后台运行的脚本，它独立于网页，可以拦截和处理网络请求，管理缓存，推送通知等。Service Worker 是实现 Progressive Web App (PWA) 的核心技术之一，它使得应用能够在离线状态下运行，并提供更好的用户体验。

### 1.1 Service Worker 的主要功能

- **网络请求拦截**：Service Worker 可以拦截网页的网络请求，并决定如何处理这些请求，例如从缓存中读取数据或从网络获取数据。
- **缓存管理**：Service Worker 可以缓存网页资源，使得应用在离线状态下仍然可以访问这些资源。
- **推送通知**：Service Worker 可以接收服务器推送的通知，并在用户设备上显示。
- **后台同步**：Service Worker 可以在后台同步数据，即使应用没有运行。

## 2. 环境准备

在开始编写 Service Worker 之前，确保你已经安装了 Node.js 和 Angular CLI。如果你还没有安装，可以通过以下命令进行安装：

```bash
# 安装 Node.js
# 访问 https://nodejs.org/ 下载并安装

# 安装 Angular CLI
npm install -g @angular/cli
```

## 3. 创建 Angular 项目

首先，使用 Angular CLI 创建一个新的 Angular 项目：

```bash
ng new my-pwa-app
cd my-pwa-app
```

## 4. 注册 Service Worker

Angular 提供了一个 `@angular/service-worker` 模块，可以方便地集成 Service Worker。首先，我们需要在项目中启用 Service Worker。

### 4.1 启用 Service Worker

在 `angular.json` 文件中，找到 `"serviceWorker"` 配置项，并将其设置为 `true`：

```json
{
  "projects": {
    "my-pwa-app": {
      "architect": {
        "build": {
          "options": {
            "serviceWorker": true
          }
        }
      }
    }
  }
}
```

### 4.2 安装 Service Worker 模块

在项目根目录下运行以下命令，安装 `@angular/service-worker` 模块：

```bash
ng add @angular/pwa
```

这个命令会自动生成 `ngsw-config.json` 配置文件，并注册 Service Worker。

### 4.3 注册 Service Worker

在 `src/app/app.module.ts` 文件中，导入 `ServiceWorkerModule` 并注册 Service Worker：

```typescript
import { NgModule } from '@angular/core';
import { BrowserModule } from '@angular/platform-browser';
import { ServiceWorkerModule } from '@angular/service-worker';
import { environment } from '../environments/environment';

import { AppComponent } from './app.component';

@NgModule({
  declarations: [
    AppComponent
  ],
  imports: [
    BrowserModule,
    ServiceWorkerModule.register('ngsw-worker.js', {
      enabled: environment.production
    })
  ],
  providers: [],
  bootstrap: [AppComponent]
})
export class AppModule { }
```

## 5. 配置 Service Worker

`ngsw-config.json` 文件用于配置 Service Worker 的行为，例如缓存策略、资源列表等。

### 5.1 缓存策略

在 `ngsw-config.json` 文件中，可以配置不同的缓存策略：

```json
{
  "index": "/index.html",
  "assetGroups": [
    {
      "name": "app",
      "installMode": "prefetch",
      "resources": {
        "files": [
          "/favicon.ico",
          "/index.html",
          "/*.css",
          "/*.js"
        ]
      }
    },
    {
      "name": "assets",
      "installMode": "lazy",
      "updateMode": "prefetch",
      "resources": {
        "files": [
          "/assets/**",
          "/*.(eot|svg|cur|jpg|png|webp|gif|otf|ttf|woff|woff2|ani)"
        ]
      }
    }
  ]
}
```

### 5.2 缓存策略解释

- **prefetch**：在安装 Service Worker 时立即缓存资源。
- **lazy**：只有在资源被请求时才缓存。

## 6. 测试 Service Worker

在开发环境中，Service Worker 默认不会启用。为了测试 Service Worker，你需要在生产环境中构建和运行应用：

```bash
ng build --prod
npx http-server -p 8080 -c-1 dist/my-pwa-app
```

打开浏览器访问 `http://localhost:8080`，然后打开开发者工具，切换到 `Application` 标签页，查看 `Service Workers` 部分，确认 Service Worker 已经注册并运行。

## 7. 实践练习

### 7.1 添加离线页面

在 `src/app/` 目录下创建一个 `offline.component.ts` 文件，并添加以下内容：

```typescript
import { Component } from '@angular/core';

@Component({
  selector: 'app-offline',
  template: `<h1>You are offline</h1>`
})
export class OfflineComponent {}
```

在 `app.module.ts` 中注册这个组件：

```typescript
import { OfflineComponent } from './offline.component';

@NgModule({
  declarations: [
    AppComponent,
    OfflineComponent
  ],
  // ...
})
export class AppModule { }
```

### 7.2 配置 Service Worker 以显示离线页面

在 `ngsw-config.json` 中添加离线页面的配置：

```json
{
  "index": "/index.html",
  "assetGroups": [
    // ...
  ],
  "dataGroups": [
    {
      "name": "offline-page",
      "urls": ["/offline"],
      "cacheConfig": {
        "maxSize": 1,
        "maxAge": "1d",
        "timeout": "5s",
        "strategy": "freshness"
      }
    }
  ]
}
```

### 7.3 测试离线页面

重新构建并运行应用：

```bash
ng build --prod
npx http-server -p 8080 -c-1 dist/my-pwa-app
```

在浏览器中访问应用，然后断开网络连接，刷新页面，你应该会看到离线页面。

## 8. 总结

通过本教程，你已经学会了如何在 Angular 项目中集成和配置 Service Worker，实现离线访问和缓存管理。Service Worker 是构建 Progressive Web App 的重要工具，掌握它将帮助你提升应用的性能和用户体验。

## 9. 进一步学习

- **推送通知**：学习如何使用 Service Worker 接收和显示推送通知。
- **后台同步**：了解如何使用 Service Worker 在后台同步数据。
- **性能优化**：探索更多 Service Worker 的缓存策略和优化技巧。

希望本教程对你有所帮助，祝你在 Angular 开发中取得更多成就！