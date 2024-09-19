---
title: 深入理解与实施CSRF保护
date: 2023-10-05
description: 本课程详细讲解CSRF攻击的原理，并提供多种有效的保护措施，帮助开发者构建更安全的Web应用。
slug: csrf-protection-course
tags:
  - 网络安全
  - Web开发
  - 安全防护
category: 编程与安全
keywords:
  - CSRF保护
  - 网络安全
  - Web应用安全
---

# CSRF 保护

## 1. 什么是 CSRF？

CSRF（Cross-Site Request Forgery，跨站请求伪造）是一种网络攻击手段，攻击者通过诱导用户在已登录的网站上执行非预期的操作，从而达到攻击目的。例如，攻击者可能会伪造一个表单提交，让用户在不知情的情况下执行转账操作。

### 1.1 CSRF 攻击的原理

- **用户登录**：用户在目标网站（如银行网站）上登录，浏览器保存了用户的会话信息（如 Cookie）。
- **恶意网站**：用户访问了一个恶意网站，该网站包含一个表单或脚本，向目标网站发送请求。
- **请求发送**：由于浏览器会自动带上目标网站的 Cookie，目标网站会认为这是用户的合法请求，从而执行操作。

## 2. CSRF 保护机制

为了防止 CSRF 攻击，Angular 提供了多种保护机制，其中最常用的是使用 CSRF 令牌（Token）。

### 2.1 CSRF 令牌的工作原理

- **生成令牌**：服务器在用户登录时生成一个唯一的 CSRF 令牌，并将其存储在用户的会话中。
- **发送令牌**：服务器将 CSRF 令牌发送给客户端，通常通过 Cookie 或隐藏表单字段。
- **验证令牌**：当客户端发送请求时，服务器会验证请求中携带的 CSRF 令牌是否与存储在会话中的令牌匹配。

### 2.2 Angular 中的 CSRF 保护

Angular 的 `HttpClient` 模块默认启用了 CSRF 保护。它会在每次请求时自动从 Cookie 中读取 CSRF 令牌，并将其作为请求头的一部分发送给服务器。

#### 2.2.1 配置 CSRF 令牌

在 Angular 中，CSRF 令牌的配置通常由服务器端完成。服务器需要在响应中设置一个名为 `XSRF-TOKEN` 的 Cookie，Angular 会自动读取并使用这个令牌。

```typescript
// 服务器端代码示例（Node.js + Express）
app.use((req, res, next) => {
  const csrfToken = generateCsrfToken(); // 生成 CSRF 令牌
  res.cookie('XSRF-TOKEN', csrfToken); // 设置 Cookie
  next();
});
```

#### 2.2.2 Angular 中的自动处理

Angular 的 `HttpClient` 模块会自动从 `XSRF-TOKEN` Cookie 中读取令牌，并将其作为 `X-XSRF-TOKEN` 请求头发送给服务器。

```typescript
// Angular 服务示例
import { HttpClient } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class ApiService {
  constructor(private http: HttpClient) {}

  postData(data: any) {
    return this.http.post('/api/data', data);
  }
}
```

### 2.3 手动配置 CSRF 令牌

在某些情况下，你可能需要手动配置 CSRF 令牌。例如，如果你使用的是第三方库或自定义的 HTTP 客户端。

```typescript
// 手动配置 CSRF 令牌
import { HttpClient, HttpHeaders } from '@angular/common/http';

@Injectable({
  providedIn: 'root'
})
export class ApiService {
  constructor(private http: HttpClient) {}

  postData(data: any) {
    const csrfToken = this.getCsrfToken(); // 获取 CSRF 令牌
    const headers = new HttpHeaders().set('X-XSRF-TOKEN', csrfToken);
    return this.http.post('/api/data', data, { headers });
  }

  private getCsrfToken(): string {
    // 从 Cookie 中获取 CSRF 令牌
    const cookies = document.cookie.split(';');
    for (const cookie of cookies) {
      const [name, value] = cookie.trim().split('=');
      if (name === 'XSRF-TOKEN') {
        return value;
      }
    }
    return '';
  }
}
```

## 3. 实践练习

### 3.1 创建一个简单的 Angular 应用

1. **创建 Angular 项目**：
   ```bash
   ng new csrf-demo
   cd csrf-demo
   ```

2. **生成服务**：
   ```bash
   ng generate service api
   ```

3. **配置服务**：
   在 `api.service.ts` 中添加以下代码：

   ```typescript
   import { Injectable } from '@angular/core';
   import { HttpClient, HttpHeaders } from '@angular/common/http';

   @Injectable({
     providedIn: 'root'
   })
   export class ApiService {
     constructor(private http: HttpClient) {}

     postData(data: any) {
       const csrfToken = this.getCsrfToken();
       const headers = new HttpHeaders().set('X-XSRF-TOKEN', csrfToken);
       return this.http.post('/api/data', data, { headers });
     }

     private getCsrfToken(): string {
       const cookies = document.cookie.split(';');
       for (const cookie of cookies) {
         const [name, value] = cookie.trim().split('=');
         if (name === 'XSRF-TOKEN') {
           return value;
         }
       }
       return '';
     }
   }
   ```

4. **使用服务**：
   在 `app.component.ts` 中使用 `ApiService`：

   ```typescript
   import { Component } from '@angular/core';
   import { ApiService } from './api.service';

   @Component({
     selector: 'app-root',
     template: `
       <button (click)="sendData()">Send Data</button>
     `
   })
   export class AppComponent {
     constructor(private apiService: ApiService) {}

     sendData() {
       this.apiService.postData({ message: 'Hello, CSRF!' }).subscribe(
         response => console.log(response),
         error => console.error(error)
       );
     }
   }
   ```

### 3.2 配置服务器端

1. **安装 Express**：
   ```bash
   npm install express
   ```

2. **创建服务器**：
   在项目根目录下创建 `server.js` 文件：

   ```javascript
   const express = require('express');
   const app = express();
   const port = 3000;

   app.use(express.json());

   app.use((req, res, next) => {
     const csrfToken = generateCsrfToken(); // 生成 CSRF 令牌
     res.cookie('XSRF-TOKEN', csrfToken); // 设置 Cookie
     next();
   });

   app.post('/api/data', (req, res) => {
     const csrfToken = req.headers['x-xsrf-token'];
     if (csrfToken === req.cookies['XSRF-TOKEN']) {
       res.json({ success: true, message: 'Data received' });
     } else {
       res.status(403).json({ success: false, message: 'CSRF token mismatch' });
     }
   });

   app.listen(port, () => {
     console.log(`Server running at http://localhost:${port}`);
   });

   function generateCsrfToken() {
     return Math.random().toString(36).substr(2);
   }
   ```

3. **启动服务器**：
   ```bash
   node server.js
   ```

### 3.3 运行应用

1. **启动 Angular 应用**：
   ```bash
   ng serve
   ```

2. **访问应用**：
   打开浏览器，访问 `http://localhost:4200`，点击按钮发送数据。

3. **检查控制台**：
   如果 CSRF 令牌匹配，服务器将返回成功响应；否则，返回错误响应。

## 4. 总结

CSRF 保护是 Web 应用安全的重要组成部分。通过使用 CSRF 令牌，Angular 和后端服务器可以有效地防止 CSRF 攻击。本教程介绍了 CSRF 攻击的原理、Angular 中的 CSRF 保护机制，并通过一个简单的实践练习展示了如何在 Angular 应用中实现 CSRF 保护。

希望本教程能帮助你更好地理解和应用 CSRF 保护技术。