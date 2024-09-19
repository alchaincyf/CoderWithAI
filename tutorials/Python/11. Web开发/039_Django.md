---
title: Django 框架入门教程
date: 2023-10-05
description: 本课程将带你从零开始学习Django框架，掌握如何使用Django构建强大的Web应用程序。
slug: django-framework-introduction
tags:
  - Django
  - Web开发
  - Python
category: 编程教程
keywords:
  - Django教程
  - Django入门
  - Python Web开发
---

# Django 框架入门

## 1. 概述

Django 是一个高级的 Python Web 框架，它鼓励快速开发和干净、实用的设计。Django 遵循 MVC（Model-View-Controller）架构，但更准确地说是 MVT（Model-View-Template）架构。Django 提供了许多内置功能，如用户认证、内容管理、表单处理、文件上传等，使得开发者可以专注于业务逻辑的实现。

## 2. 环境搭建

### 2.1 安装 Python

确保你的系统上已经安装了 Python。你可以通过以下命令检查 Python 版本：

```bash
python --version
```

如果没有安装，请访问 [Python 官方网站](https://www.python.org/) 下载并安装。

### 2.2 安装 Django

使用 pip 安装 Django：

```bash
pip install django
```

### 2.3 创建虚拟环境

为了隔离项目依赖，建议创建一个虚拟环境：

```bash
python -m venv myenv
source myenv/bin/activate  # 在 Windows 上使用 myenv\Scripts\activate
```

## 3. 创建第一个 Django 项目

### 3.1 创建项目

使用 Django 提供的命令行工具创建一个新的项目：

```bash
django-admin startproject mysite
```

这将在当前目录下创建一个名为 `mysite` 的目录，包含以下文件：

```
mysite/
    manage.py
    mysite/
        __init__.py
        settings.py
        urls.py
        asgi.py
        wsgi.py
```

### 3.2 运行开发服务器

进入项目目录并启动开发服务器：

```bash
cd mysite
python manage.py runserver
```

打开浏览器，访问 `http://127.0.0.1:8000/`，你应该会看到 Django 的欢迎页面。

## 4. 创建应用

### 4.1 创建应用

在 Django 中，项目可以包含多个应用。每个应用负责不同的功能模块。创建一个名为 `blog` 的应用：

```bash
python manage.py startapp blog
```

### 4.2 定义模型

在 `blog/models.py` 中定义一个简单的模型：

```python
from django.db import models

class Post(models.Model):
    title = models.CharField(max_length=200)
    content = models.TextField()
    pub_date = models.DateTimeField('date published')

    def __str__(self):
        return self.title
```

### 4.3 激活模型

在 `mysite/settings.py` 中添加 `blog` 应用：

```python
INSTALLED_APPS = [
    ...
    'blog',
]
```

然后运行迁移命令来创建数据库表：

```bash
python manage.py makemigrations
python manage.py migrate
```

## 5. 创建视图和模板

### 5.1 创建视图

在 `blog/views.py` 中创建一个简单的视图：

```python
from django.shortcuts import render
from .models import Post

def post_list(request):
    posts = Post.objects.order_by('-pub_date')
    return render(request, 'blog/post_list.html', {'posts': posts})
```

### 5.2 创建模板

在 `blog/templates/blog/` 目录下创建 `post_list.html` 文件：

```html
<!DOCTYPE html>
<html>
<head>
    <title>Blog Posts</title>
</head>
<body>
    <h1>Blog Posts</h1>
    <ul>
    {% for post in posts %}
        <li>{{ post.title }} - {{ post.pub_date }}</li>
    {% endfor %}
    </ul>
</body>
</html>
```

### 5.3 配置 URL

在 `blog/urls.py` 中配置 URL：

```python
from django.urls import path
from . import views

urlpatterns = [
    path('', views.post_list, name='post_list'),
]
```

然后在 `mysite/urls.py` 中包含 `blog` 应用的 URL：

```python
from django.contrib import admin
from django.urls import include, path

urlpatterns = [
    path('admin/', admin.site.urls),
    path('blog/', include('blog.urls')),
]
```

## 6. 运行项目

再次运行开发服务器：

```bash
python manage.py runserver
```

访问 `http://127.0.0.1:8000/blog/`，你应该会看到博客文章的列表。

## 7. 实践练习

### 7.1 添加博客文章

1. 使用 Django 的管理界面添加几篇博客文章。
2. 访问 `http://127.0.0.1:8000/blog/` 查看文章列表。

### 7.2 扩展功能

1. 为博客文章添加评论功能。
2. 实现博客文章的详细页面。

## 8. 总结

通过本教程，你已经学会了如何创建一个简单的 Django 项目，并实现了一个基本的博客应用。Django 提供了丰富的功能和工具，帮助你快速开发 Web 应用。继续探索 Django 的文档和社区资源，进一步提升你的开发技能。

## 9. 参考资料

- [Django 官方文档](https://docs.djangoproject.com/en/stable/)
- [Django 教程 - 官方](https://docs.djangoproject.com/en/stable/intro/tutorial01/)
- [Django 社区](https://www.djangoproject.com/community/)

希望这篇教程能帮助你顺利入门 Django 框架，开启你的 Web 开发之旅！