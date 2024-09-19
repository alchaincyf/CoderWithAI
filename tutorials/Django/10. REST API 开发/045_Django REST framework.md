---
title: Django REST framework 入门教程
date: 2023-10-05
description: 本课程将带你从零开始学习如何使用Django REST framework构建强大的Web API。涵盖基础概念、序列化、视图、路由以及认证和权限管理。
slug: django-rest-framework-tutorial
tags:
  - Django
  - REST API
  - Web开发
category: 编程教程
keywords:
  - Django REST framework
  - REST API
  - Django教程
---

# Django REST framework 入门

## 概述

Django REST framework (DRF) 是一个强大的工具，用于构建 Web API。它建立在 Django 之上，提供了丰富的功能和灵活性，使得开发 RESTful API 变得简单而高效。本教程将带你从零开始，逐步掌握 Django REST framework 的核心概念和使用方法。

## 安装和环境设置

### 安装 Django 和 Django REST framework

首先，确保你已经安装了 Python 和 Django。如果还没有安装 Django，可以使用以下命令进行安装：

```bash
pip install django
```

接下来，安装 Django REST framework：

```bash
pip install djangorestframework
```

### 创建 Django 项目

使用以下命令创建一个新的 Django 项目：

```bash
django-admin startproject myproject
```

进入项目目录：

```bash
cd myproject
```

创建一个新的 Django 应用：

```bash
python manage.py startapp myapp
```

### 配置 Django REST framework

在 `myproject/settings.py` 文件中，将 `rest_framework` 添加到 `INSTALLED_APPS` 列表中：

```python
INSTALLED_APPS = [
    ...
    'rest_framework',
    'myapp',
]
```

## 序列化器

### 什么是序列化器？

序列化器（Serializer）是 Django REST framework 的核心组件之一。它负责将复杂的 Django 模型数据转换为 JSON 或其他格式，以便在 API 中传输。

### 创建序列化器

在 `myapp` 目录下创建一个名为 `serializers.py` 的文件，并添加以下代码：

```python
from rest_framework import serializers
from .models import MyModel

class MyModelSerializer(serializers.ModelSerializer):
    class Meta:
        model = MyModel
        fields = '__all__'
```

### 定义模型

在 `myapp/models.py` 中定义一个简单的模型：

```python
from django.db import models

class MyModel(models.Model):
    name = models.CharField(max_length=100)
    description = models.TextField()
```

### 迁移数据库

运行以下命令创建数据库表：

```bash
python manage.py makemigrations
python manage.py migrate
```

## 视图集和路由器

### 什么是视图集？

视图集（ViewSet）是 Django REST framework 中用于处理 API 请求的类。它结合了多个视图（如 `list`、`create`、`retrieve`、`update`、`destroy`）到一个类中，简化了 API 的开发。

### 创建视图集

在 `myapp/views.py` 中创建一个视图集：

```python
from rest_framework import viewsets
from .models import MyModel
from .serializers import MyModelSerializer

class MyModelViewSet(viewsets.ModelViewSet):
    queryset = MyModel.objects.all()
    serializer_class = MyModelSerializer
```

### 配置路由

在 `myapp/urls.py` 中配置路由：

```python
from django.urls import path, include
from rest_framework.routers import DefaultRouter
from .views import MyModelViewSet

router = DefaultRouter()
router.register(r'mymodels', MyModelViewSet)

urlpatterns = [
    path('', include(router.urls)),
]
```

### 项目路由配置

在 `myproject/urls.py` 中包含 `myapp` 的路由：

```python
from django.contrib import admin
from django.urls import path, include

urlpatterns = [
    path('admin/', admin.site.urls),
    path('api/', include('myapp.urls')),
]
```

## 运行服务器

启动 Django 开发服务器：

```bash
python manage.py runserver
```

访问 `http://127.0.0.1:8000/api/mymodels/`，你应该能够看到一个空的列表，因为还没有创建任何 `MyModel` 实例。

## 实践练习

### 创建数据

使用 Django 的 shell 创建一些 `MyModel` 实例：

```bash
python manage.py shell
```

在 shell 中输入以下代码：

```python
from myapp.models import MyModel

MyModel.objects.create(name="Example 1", description="This is an example.")
MyModel.objects.create(name="Example 2", description="This is another example.")
```

### 查看 API 响应

刷新浏览器页面，你应该能够看到刚刚创建的 `MyModel` 实例。

## 总结

通过本教程，你已经学会了如何使用 Django REST framework 创建一个简单的 RESTful API。你了解了序列化器、视图集和路由器的使用方法，并进行了实际操作。接下来，你可以继续深入学习 Django REST framework 的其他高级功能，如认证、权限、API 版本控制等。

## 下一步

- 学习 Django REST framework 的认证和权限系统。
- 探索 API 版本控制和单元测试。
- 实践更复杂的 API 设计和实现。

希望本教程对你有所帮助，祝你在 Django REST framework 的学习和实践中取得成功！