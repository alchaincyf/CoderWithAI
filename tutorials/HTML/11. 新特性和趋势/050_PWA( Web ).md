---
title: PWA（渐进式 Web 应用）基础教程
date: 2023-10-05
description: 本课程将带你深入了解PWA（渐进式Web应用）的基础知识，包括Service Workers、Manifest文件、离线缓存等关键技术，帮助你构建高性能、可靠的Web应用。
slug: pwa-basics
tags:
  - PWA
  - Web开发
  - 前端技术
category: 前端开发
keywords:
  - PWA基础
  - 渐进式Web应用
  - Service Workers
  - Web应用开发
---

# PWA（渐进式 Web 应用）基础

## 概述

渐进式 Web 应用（PWA）是一种结合了 Web 和原生应用优点的应用模型。PWA 可以在任何设备上运行，并且具有离线访问、推送通知、主屏幕图标等功能。本教程将带你了解 PWA 的基础知识，包括其核心概念、技术栈以及如何构建一个简单的 PWA。

## 1. PWA 的核心概念

### 1.1 什么是 PWA？

PWA 是一种 Web 应用，它使用现代 Web API 和传统的渐进式增强策略来提供类似于原生应用的用户体验。PWA 具有以下特点：

- **渐进式**：适用于所有浏览器，无论新旧。
- **响应式**：适应任何设备和屏幕尺寸。
- **离线可用**：通过 Service Worker 实现离线访问。
- **类似原生应用**：可以添加到主屏幕，提供类似原生应用的体验。
- **持续更新**：通过 Service Worker 实现自动更新。
- **安全**：通过 HTTPS 提供服务，确保数据安全。
- **可发现**：通过搜索引擎优化（SEO）提高可见性。
- **可重连**：通过推送通知等功能保持用户参与度。

### 1.2 PWA 的技术栈

构建 PWA 需要以下技术：

- **HTML/CSS/JavaScript**：基本的 Web 技术。
- **Service Worker**：用于实现离线访问和缓存。
- **Web App Manifest**：用于定义应用的元数据和启动方式。
- **HTTPS**：确保应用的安全性。

## 2. 构建一个简单的 PWA

### 2.1 创建基本的 HTML 页面

首先，我们创建一个简单的 HTML 页面。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>My First PWA</title>
    <link rel="stylesheet" href="styles.css">
</head>
<body>
    <h1>Welcome to My PWA</h1>
    <p>This is a simple Progressive Web App.</p>
</body>
</html>
```

### 2.2 添加 Web App Manifest

Web App Manifest 是一个 JSON 文件，用于定义应用的元数据和启动方式。

创建 `manifest.json` 文件：

```json
{
    "name": "My First PWA",
    "short_name": "PWA",
    "start_url": "/",
    "display": "standalone",
    "background_color": "#ffffff",
    "theme_color": "#000000",
    "icons": [
        {
            "src": "icon.png",
            "sizes": "192x192",
            "type": "image/png"
        }
    ]
}
```

在 HTML 文件中引用 `manifest.json`：

```html
<link rel="manifest" href="manifest.json">
```

### 2.3 注册 Service Worker

Service Worker 是 PWA 的核心组件，用于实现离线访问和缓存。

创建 `service-worker.js` 文件：

```javascript
self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open('my-pwa-cache').then((cache) => {
            return cache.addAll([
                '/',
                '/styles.css',
                '/icon.png'
            ]);
        })
    );
});

self.addEventListener('fetch', (event) => {
    event.respondWith(
        caches.match(event.request).then((response) => {
            return response || fetch(event.request);
        })
    );
});
```

在 HTML 文件中注册 Service Worker：

```html
<script>
    if ('serviceWorker' in navigator) {
        window.addEventListener('load', () => {
            navigator.serviceWorker.register('/service-worker.js')
                .then((registration) => {
                    console.log('Service Worker registered with scope:', registration.scope);
                })
                .catch((error) => {
                    console.log('Service Worker registration failed:', error);
                });
        });
    }
</script>
```

### 2.4 测试 PWA

在浏览器中打开你的 HTML 文件，然后使用开发者工具检查 Service Worker 是否已注册，并测试离线访问功能。

## 3. 实践练习

### 3.1 添加离线页面

创建一个离线页面 `offline.html`，并在 Service Worker 中添加缓存规则。

```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Offline</title>
</head>
<body>
    <h1>You are offline</h1>
    <p>Please check your internet connection.</p>
</body>
</html>
```

在 `service-worker.js` 中添加离线页面的缓存规则：

```javascript
self.addEventListener('install', (event) => {
    event.waitUntil(
        caches.open('my-pwa-cache').then((cache) => {
            return cache.addAll([
                '/',
                '/styles.css',
                '/icon.png',
                '/offline.html'
            ]);
        })
    );
});
```

### 3.2 添加推送通知

使用 Web Push API 实现推送通知功能。

```javascript
self.addEventListener('push', (event) => {
    const title = 'New Notification';
    const options = {
        body: 'This is a push notification.',
        icon: '/icon.png'
    };

    event.waitUntil(self.registration.showNotification(title, options));
});
```

## 4. 总结

通过本教程，你已经了解了 PWA 的核心概念和技术栈，并学会了如何构建一个简单的 PWA。PWA 提供了许多原生应用的功能，同时保持了 Web 应用的灵活性和可访问性。继续探索 PWA 的高级功能，如推送通知、后台同步等，进一步提升你的 Web 应用体验。

## 5. 进一步学习资源

- [MDN Web 文档 - Progressive Web Apps](https://developer.mozilla.org/en-US/docs/Web/Progressive_web_apps)
- [Google Developers - PWA Training](https://developers.google.com/web/ilt/pwa)
- [Service Worker Cookbook](https://serviceworke.rs/)

通过这些资源，你可以深入学习 PWA 的更多高级功能和最佳实践。