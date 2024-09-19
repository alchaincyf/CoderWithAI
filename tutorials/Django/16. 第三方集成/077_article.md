---
title: 邮件发送编程教程
date: 2023-10-05
description: 本课程详细讲解如何使用Python和SMTP协议发送电子邮件，包括设置邮件服务器、编写邮件内容和处理常见错误。
slug: email-sending-programming-tutorial
tags:
  - Python
  - SMTP
  - 电子邮件
category: 编程教程
keywords:
  - 邮件发送
  - Python邮件
  - SMTP协议
---

# Django 邮件发送教程

## 1. 简介

在现代 Web 应用中，邮件发送是一个常见的需求。无论是用户注册确认、密码重置，还是通知用户某些事件，邮件都是一种有效的沟通方式。Django 提供了一个强大的邮件发送框架，使得在应用中集成邮件功能变得非常简单。

本教程将详细介绍如何在 Django 中配置和使用邮件发送功能。我们将从基础的邮件发送开始，逐步深入到更复杂的场景，如发送 HTML 邮件和附件。

## 2. 配置邮件发送

### 2.1 设置邮件后端

Django 支持多种邮件后端，包括 SMTP、文件、控制台等。默认情况下，Django 使用 SMTP 后端。你可以在 `settings.py` 文件中配置邮件后端。

```python
# settings.py

EMAIL_BACKEND = 'django.core.mail.backends.smtp.EmailBackend'
EMAIL_HOST = 'smtp.example.com'  # 你的 SMTP 服务器地址
EMAIL_PORT = 587  # SMTP 端口，通常是 587 或 465
EMAIL_USE_TLS = True  # 是否使用 TLS 加密
EMAIL_HOST_USER = 'your_email@example.com'  # 你的邮箱地址
EMAIL_HOST_PASSWORD = 'your_email_password'  # 你的邮箱密码
DEFAULT_FROM_EMAIL = 'your_email@example.com'  # 默认发件人地址
```

### 2.2 使用控制台后端进行开发测试

在开发过程中，为了避免频繁发送真实邮件，可以使用控制台后端。控制台后端会将邮件内容输出到控制台，而不是实际发送邮件。

```python
# settings.py

EMAIL_BACKEND = 'django.core.mail.backends.console.EmailBackend'
```

## 3. 发送简单邮件

### 3.1 发送纯文本邮件

Django 提供了一个 `send_mail` 函数，用于发送纯文本邮件。以下是一个简单的示例：

```python
from django.core.mail import send_mail

send_mail(
    'Subject here',  # 邮件主题
    'Here is the message.',  # 邮件内容
    'from@example.com',  # 发件人地址
    ['to@example.com'],  # 收件人地址列表
    fail_silently=False,  # 是否在发送失败时抛出异常
)
```

### 3.2 发送 HTML 邮件

Django 还支持发送 HTML 格式的邮件。你可以使用 `EmailMultiAlternatives` 类来发送包含 HTML 内容的邮件。

```python
from django.core.mail import EmailMultiAlternatives

subject, from_email, to = 'hello', 'from@example.com', 'to@example.com'
text_content = 'This is an important message.'
html_content = '<p>This is an <strong>important</strong> message.</p>'
msg = EmailMultiAlternatives(subject, text_content, from_email, [to])
msg.attach_alternative(html_content, "text/html")
msg.send()
```

## 4. 发送带附件的邮件

### 4.1 添加附件

你可以使用 `EmailMessage` 类来发送带附件的邮件。以下是一个示例：

```python
from django.core.mail import EmailMessage

email = EmailMessage(
    'Subject',  # 邮件主题
    'Message.',  # 邮件内容
    'from@example.com',  # 发件人地址
    ['to@example.com'],  # 收件人地址列表
)

# 添加附件
email.attach_file('/path/to/file.pdf')

email.send()
```

### 4.2 添加多个附件

你可以多次调用 `attach_file` 方法来添加多个附件。

```python
email.attach_file('/path/to/another_file.pdf')
email.attach_file('/path/to/image.jpg')
```

## 5. 实践练习

### 5.1 练习：发送注册确认邮件

在你的 Django 项目中，创建一个用户注册视图，并在用户注册成功后发送一封确认邮件。邮件内容可以是一个简单的欢迎信息，并包含一个确认链接。

### 5.2 练习：发送带附件的通知邮件

创建一个视图，用于生成并发送一份包含用户活动报告的邮件。报告可以是一个 PDF 文件，作为邮件的附件发送。

## 6. 总结

通过本教程，你已经学会了如何在 Django 中配置和使用邮件发送功能。从简单的纯文本邮件到复杂的带附件的 HTML 邮件，Django 提供了丰富的工具来满足各种邮件发送需求。希望你能将这些知识应用到实际项目中，提升用户体验和应用功能。

## 7. 进一步学习

- **邮件模板**：学习如何使用 Django 模板系统来生成邮件内容。
- **异步邮件发送**：了解如何使用 Celery 等工具实现异步邮件发送，避免阻塞请求。
- **邮件队列**：探索如何使用 Redis 或 RabbitMQ 等工具实现邮件队列，提高邮件发送的可靠性和效率。

继续探索 Django 的强大功能，让你的 Web 应用更加完善！