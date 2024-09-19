---
title: CSRF 防御详解
date: 2023-10-05
description: 本课程详细讲解跨站请求伪造（CSRF）攻击的原理及其防御措施，帮助开发者提升Web应用的安全性。
slug: csrf-defense-tutorial
tags:
  - 网络安全
  - Web开发
  - 安全防御
category: 编程安全
keywords:
  - CSRF防御
  - 跨站请求伪造
  - Web安全
---

# CSRF 防御

## 1. 什么是 CSRF？

CSRF（Cross-Site Request Forgery，跨站请求伪造）是一种网络攻击方式，攻击者通过诱导用户在已登录的网站上执行非预期的操作，从而达到攻击目的。例如，攻击者可能会诱使用户点击一个链接，该链接会自动向用户已登录的银行网站发送转账请求。

### 1.1 CSRF 攻击的原理

CSRF 攻击的原理是利用用户在已登录的网站上的会话状态，通过伪造请求来执行恶意操作。攻击者通常会通过社交工程手段诱使用户点击恶意链接或访问恶意网站，从而触发这些伪造的请求。

### 1.2 CSRF 攻击的危害

CSRF 攻击可能导致以下危害：

- 用户账户被盗用
- 用户数据被篡改
- 用户资金被转移

## 2. CSRF 防御策略

为了防止 CSRF 攻击，开发者可以采取以下几种防御策略：

### 2.1 使用 CSRF Token

CSRF Token 是一种常见的防御机制。服务器在生成表单时，会为每个表单生成一个唯一的 Token，并将该 Token 存储在用户的会话中。当用户提交表单时，服务器会验证表单中的 Token 是否与存储在会话中的 Token 一致。如果不一致，则拒绝请求。

#### 2.1.1 生成 CSRF Token

在服务器端生成 CSRF Token 的示例代码：

```javascript
const crypto = require('crypto');

function generateCsrfToken() {
  return crypto.randomBytes(16).toString('hex');
}

const csrfToken = generateCsrfToken();
console.log(csrfToken);
```

#### 2.1.2 在表单中嵌入 CSRF Token

在 HTML 表单中嵌入 CSRF Token 的示例代码：

```html
<form action="/submit" method="POST">
  <input type="hidden" name="csrfToken" value="your-csrf-token-here">
  <input type="text" name="username">
  <input type="password" name="password">
  <button type="submit">Submit</button>
</form>
```

#### 2.1.3 验证 CSRF Token

在服务器端验证 CSRF Token 的示例代码：

```javascript
const express = require('express');
const app = express();

app.use(express.urlencoded({ extended: true }));

app.post('/submit', (req, res) => {
  const csrfToken = req.body.csrfToken;
  const sessionCsrfToken = req.session.csrfToken;

  if (csrfToken !== sessionCsrfToken) {
    return res.status(403).send('CSRF token validation failed');
  }

  // Proceed with the request
  res.send('Form submitted successfully');
});

app.listen(3000, () => {
  console.log('Server is running on port 3000');
});
```

### 2.2 使用 SameSite Cookie 属性

SameSite 是 Cookie 的一个属性，用于控制 Cookie 在跨站请求中的发送行为。通过设置 SameSite 属性，可以有效防止 CSRF 攻击。

#### 2.2.1 SameSite 属性的值

- `Strict`: Cookie 仅在同站请求中发送。
- `Lax`: Cookie 在同站请求和部分跨站请求中发送（例如 GET 请求）。
- `None`: Cookie 在所有请求中发送（需要设置 `Secure` 属性）。

#### 2.2.2 设置 SameSite 属性

在 Express 中设置 SameSite 属性的示例代码：

```javascript
const express = require('express');
const cookieParser = require('cookie-parser');
const app = express();

app.use(cookieParser());

app.get('/set-cookie', (req, res) => {
  res.cookie('sessionId', '123456', {
    httpOnly: true,
    sameSite: 'Strict', // 或者 'Lax' 或 'None'
    secure: true // 如果 SameSite 为 'None'，则必须设置 secure 属性
  });
  res.send('Cookie set');
});

app.listen(3000, () => {
  console.log('Server is running on port 3000');
});
```

### 2.3 使用双重提交 Cookie

双重提交 Cookie 是一种结合 CSRF Token 和 Cookie 的防御机制。服务器在生成 CSRF Token 时，会将 Token 同时存储在 Cookie 和表单中。当用户提交表单时，服务器会验证表单中的 Token 是否与 Cookie 中的 Token 一致。

#### 2.3.1 生成双重提交 Cookie

在服务器端生成双重提交 Cookie 的示例代码：

```javascript
const express = require('express');
const cookieParser = require('cookie-parser');
const app = express();

app.use(cookieParser());

app.get('/set-cookie', (req, res) => {
  const csrfToken = generateCsrfToken();
  res.cookie('csrfToken', csrfToken, { httpOnly: true });
  res.send(`
    <form action="/submit" method="POST">
      <input type="hidden" name="csrfToken" value="${csrfToken}">
      <input type="text" name="username">
      <input type="password" name="password">
      <button type="submit">Submit</button>
    </form>
  `);
});

app.listen(3000, () => {
  console.log('Server is running on port 3000');
});
```

#### 2.3.2 验证双重提交 Cookie

在服务器端验证双重提交 Cookie 的示例代码：

```javascript
app.post('/submit', (req, res) => {
  const csrfToken = req.body.csrfToken;
  const cookieCsrfToken = req.cookies.csrfToken;

  if (csrfToken !== cookieCsrfToken) {
    return res.status(403).send('CSRF token validation failed');
  }

  // Proceed with the request
  res.send('Form submitted successfully');
});
```

## 3. 实践练习

### 3.1 创建一个简单的表单应用

1. 使用 Express 创建一个简单的表单应用。
2. 在表单中嵌入 CSRF Token。
3. 在服务器端验证 CSRF Token。

### 3.2 设置 SameSite Cookie

1. 修改上述表单应用，设置 SameSite Cookie 属性。
2. 测试不同 SameSite 属性值对 Cookie 发送行为的影响。

### 3.3 实现双重提交 Cookie

1. 修改上述表单应用，实现双重提交 Cookie 机制。
2. 验证双重提交 Cookie 的有效性。

## 4. 总结

CSRF 攻击是一种常见的网络攻击方式，但通过使用 CSRF Token、SameSite Cookie 属性和双重提交 Cookie 等防御策略，可以有效防止 CSRF 攻击。开发者应根据具体应用场景选择合适的防御策略，并结合其他安全措施，确保应用的安全性。

## 5. 进一步学习

- 深入了解其他安全攻击方式，如 XSS（跨站脚本攻击）。
- 学习如何在 React 应用中集成 CSRF 防御机制。
- 探索其他安全框架和工具，如 OWASP 提供的安全指南。

通过本教程的学习，你应该能够理解 CSRF 攻击的原理，并掌握几种常见的 CSRF 防御策略。希望这些知识能够帮助你在实际开发中构建更安全的应用。