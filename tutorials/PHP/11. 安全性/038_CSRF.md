---
title: CSRF 防御详解
date: 2023-10-05
description: 本课程详细讲解跨站请求伪造（CSRF）攻击的原理及防御措施，帮助开发者构建更安全的Web应用。
slug: csrf-defense-tutorial
tags:
  - 网络安全
  - Web开发
  - 安全策略
category: 网络安全
keywords:
  - CSRF防御
  - 跨站请求伪造
  - Web安全
---

# CSRF 防御

## 概述

跨站请求伪造（CSRF）是一种网络攻击，攻击者诱使用户在已登录的网站上执行非预期的操作。为了防止这种攻击，我们需要在应用程序中实施有效的CSRF防御机制。

## 理论解释

### 什么是CSRF？

CSRF攻击利用用户在已登录的网站上的身份验证信息，诱使用户在不知情的情况下执行某些操作。例如，攻击者可能通过电子邮件或恶意网站诱使用户点击链接，从而在用户不知情的情况下执行转账操作。

### CSRF攻击的原理

1. **用户登录网站**：用户在网站A上登录，网站A在用户的浏览器中存储了会话信息（如Cookie）。
2. **用户访问恶意网站**：用户访问了攻击者控制的网站B。
3. **恶意请求**：网站B包含一个表单或链接，该表单或链接会向网站A发送请求，执行某些操作（如转账）。
4. **用户执行操作**：由于用户在网站A上已登录，浏览器会自动发送包含会话信息的请求，导致操作被执行。

### CSRF防御机制

1. **CSRF令牌**：在每个表单中添加一个唯一的CSRF令牌，并在服务器端验证该令牌。
2. **SameSite Cookie属性**：设置Cookie的SameSite属性为`Strict`或`Lax`，防止跨站请求携带Cookie。
3. **验证HTTP Referer头**：检查请求的Referer头，确保请求来自合法的来源。

## 代码示例

### 生成和验证CSRF令牌

```php
<?php
session_start();

// 生成CSRF令牌
if (!isset($_SESSION['csrf_token'])) {
    $_SESSION['csrf_token'] = bin2hex(random_bytes(32));
}

// 表单示例
?>
<form method="POST" action="process.php">
    <input type="hidden" name="csrf_token" value="<?php echo $_SESSION['csrf_token']; ?>">
    <input type="text" name="amount" placeholder="转账金额">
    <button type="submit">转账</button>
</form>

<?php
// 验证CSRF令牌
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    if (!hash_equals($_SESSION['csrf_token'], $_POST['csrf_token'])) {
        die('无效的CSRF令牌');
    }
    // 处理转账逻辑
    $amount = $_POST['amount'];
    // ...
}
?>
```

### 设置SameSite Cookie属性

```php
<?php
// 设置Cookie时添加SameSite属性
setcookie('session_id', '12345', [
    'expires' => time() + 3600,
    'path' => '/',
    'domain' => 'example.com',
    'secure' => true,
    'httponly' => true,
    'samesite' => 'Strict'
]);
?>
```

### 验证HTTP Referer头

```php
<?php
if ($_SERVER['REQUEST_METHOD'] === 'POST') {
    $referer = $_SERVER['HTTP_REFERER'];
    if (parse_url($referer, PHP_URL_HOST) !== 'example.com') {
        die('非法请求');
    }
    // 处理请求
}
?>
```

## 实践练习

### 练习1：实现CSRF令牌

1. 创建一个简单的表单，包含一个隐藏的CSRF令牌字段。
2. 在服务器端生成并存储CSRF令牌。
3. 在处理表单提交时，验证CSRF令牌。

### 练习2：设置SameSite Cookie属性

1. 修改现有的Cookie设置代码，添加SameSite属性。
2. 测试Cookie是否在跨站请求中被发送。

### 练习3：验证HTTP Referer头

1. 在处理敏感操作的代码中，添加对HTTP Referer头的验证。
2. 测试不同来源的请求，观察验证结果。

## 总结

CSRF攻击是一种常见的安全威胁，但通过实施CSRF令牌、SameSite Cookie属性和HTTP Referer头验证等防御机制，我们可以有效地保护应用程序免受此类攻击。通过本教程的学习和实践练习，你应该能够理解并实现这些防御措施。

## 进一步学习

- 深入了解其他Web安全威胁，如XSS和SQL注入。
- 学习如何在框架（如Laravel、Symfony）中集成CSRF防御。
- 探索更高级的安全策略，如OAuth和JWT认证。

希望本教程对你理解CSRF防御有所帮助，祝你在编程学习中取得更多进步！