---
title: 跨站请求伪造 (CSRF) 防御教程
date: 2023-10-05
description: 本课程详细讲解跨站请求伪造 (CSRF) 攻击的原理及如何通过多种方法有效防御，确保Web应用的安全性。
slug: csrf-defense-tutorial
tags:
  - 网络安全
  - Web安全
  - CSRF防御
category: 编程安全
keywords:
  - CSRF
  - 跨站请求伪造
  - 网络安全
  - Web应用安全
  - CSRF防御方法
---

# 跨站请求伪造 (CSRF) 防御

## 1. 什么是跨站请求伪造 (CSRF)？

跨站请求伪造（CSRF）是一种网络攻击，攻击者诱使用户在已登录的网站上执行非预期的操作。攻击者通过伪造请求，利用用户在目标网站上的身份验证信息，执行恶意操作。

### 1.1 CSRF 攻击的原理

- **用户登录**：用户在目标网站（如银行网站）上登录并保持会话。
- **恶意网站**：用户访问一个恶意网站，该网站包含一个表单或脚本，向目标网站发送请求。
- **伪造请求**：恶意网站利用用户的浏览器发送请求到目标网站，由于用户已登录，目标网站会处理该请求。

### 1.2 CSRF 攻击的例子

假设用户在银行网站上登录，并保持会话。攻击者创建一个恶意网站，包含以下 HTML 代码：

```html
<form action="https://bank.com/transfer" method="POST">
  <input type="hidden" name="amount" value="1000">
  <input type="hidden" name="toAccount" value="attackerAccount">
</form>
<script>
  document.forms[0].submit();
</script>
```

当用户访问恶意网站时，浏览器会自动提交表单，向银行网站发送转账请求，导致用户账户资金被转移。

## 2. CSRF 防御策略

### 2.1 使用 CSRF Token

CSRF Token 是一种随机生成的字符串，服务器在每个表单中嵌入该 Token，并在处理请求时验证该 Token。

#### 2.1.1 生成 CSRF Token

服务器在用户登录时生成一个 CSRF Token，并将其存储在会话中。

```javascript
// 服务器端代码 (Node.js)
const crypto = require('crypto');

function generateCSRFToken() {
  return crypto.randomBytes(16).toString('hex');
}

app.use((req, res, next) => {
  if (!req.session.csrfToken) {
    req.session.csrfToken = generateCSRFToken();
  }
  next();
});
```

#### 2.1.2 嵌入 CSRF Token 到表单

在表单中嵌入 CSRF Token：

```html
<form action="/transfer" method="POST">
  <input type="hidden" name="csrfToken" value="{{ csrfToken }}">
  <input type="text" name="amount" placeholder="Amount">
  <input type="text" name="toAccount" placeholder="To Account">
  <button type="submit">Transfer</button>
</form>
```

#### 2.1.3 验证 CSRF Token

服务器在处理请求时验证 CSRF Token：

```javascript
app.post('/transfer', (req, res) => {
  const csrfToken = req.body.csrfToken;
  if (csrfToken !== req.session.csrfToken) {
    return res.status(403).send('Invalid CSRF Token');
  }
  // 处理转账请求
});
```

### 2.2 使用 SameSite Cookie 属性

SameSite 属性可以防止浏览器在跨站请求中发送 Cookie。

```javascript
// 设置 SameSite 属性
res.cookie('sessionId', sessionId, {
  httpOnly: true,
  secure: true,
  sameSite: 'strict'
});
```

### 2.3 验证请求来源 (Referer 和 Origin)

服务器可以验证请求的 Referer 和 Origin 头，确保请求来自预期的来源。

```javascript
app.post('/transfer', (req, res) => {
  const referer = req.headers.referer;
  const origin = req.headers.origin;
  if (!referer || !origin || !referer.startsWith('https://bank.com') || !origin.startsWith('https://bank.com')) {
    return res.status(403).send('Invalid Referer or Origin');
  }
  // 处理转账请求
});
```

## 3. 实践练习

### 3.1 创建一个简单的转账表单

1. 创建一个 HTML 表单，包含转账金额和目标账户字段。
2. 在服务器端生成并嵌入 CSRF Token。
3. 在服务器端验证 CSRF Token。

### 3.2 实现 SameSite Cookie 属性

1. 在服务器端设置 SameSite 属性为 `strict`。
2. 测试跨站请求是否能够发送 Cookie。

### 3.3 验证 Referer 和 Origin

1. 在服务器端验证请求的 Referer 和 Origin 头。
2. 测试跨站请求是否能够通过验证。

## 4. 总结

跨站请求伪造 (CSRF) 是一种常见的安全威胁，但通过使用 CSRF Token、SameSite Cookie 属性和验证请求来源，可以有效防御 CSRF 攻击。开发者应始终保持警惕，确保应用程序的安全性。

## 5. 进一步学习

- 深入了解其他安全威胁，如跨站脚本 (XSS)。
- 学习如何使用安全框架和库来简化安全实践。
- 参与安全社区，了解最新的安全漏洞和防御技术。

通过本教程，您应该已经掌握了 CSRF 防御的基本概念和实践方法。继续学习和实践，确保您的应用程序安全可靠。