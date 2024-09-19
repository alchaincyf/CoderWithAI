---
title: 自定义用户模型 - Django 高级教程
date: 2023-10-05
description: 本课程深入探讨如何在Django中创建和自定义用户模型，包括扩展内置用户模型和使用自定义用户模型的好处。
slug: custom-user-model-django
tags:
  - Django
  - 自定义用户模型
  - 后端开发
category: 后端开发
keywords:
  - Django 自定义用户模型
  - Django 用户扩展
  - Django 用户认证
---

# 自定义用户模型

在Django中，默认的用户模型提供了基本的用户认证功能，但在实际项目中，我们可能需要扩展或自定义用户模型以满足特定的需求。例如，添加额外的字段、更改认证方式等。本教程将详细介绍如何在Django中自定义用户模型。

## 1. 为什么要自定义用户模型？

Django的默认用户模型包含以下字段：
- `username`
- `password`
- `email`
- `first_name`
- `last_name`

然而，在实际项目中，我们可能需要：
- 添加额外的用户信息（如电话号码、地址等）。
- 使用不同的字段进行用户认证（如使用电子邮件地址代替用户名）。
- 更改用户模型的行为。

自定义用户模型可以灵活地满足这些需求。

## 2. 自定义用户模型的步骤

### 2.1 创建一个新的Django项目

首先，确保你已经安装了Django。如果没有，可以使用以下命令安装：

```bash
pip install django
```

然后，创建一个新的Django项目：

```bash
django-admin startproject myproject
```

进入项目目录：

```bash
cd myproject
```

### 2.2 创建一个新的应用

在项目中创建一个新的应用：

```bash
python manage.py startapp accounts
```

### 2.3 定义自定义用户模型

在`accounts`应用的`models.py`文件中，定义一个新的用户模型。我们将继承Django的`AbstractUser`类，并添加一些自定义字段。

```python
from django.contrib.auth.models import AbstractUser
from django.db import models

class CustomUser(AbstractUser):
    # 添加自定义字段
    phone_number = models.CharField(max_length=15, blank=True, null=True)
    address = models.TextField(blank=True, null=True)

    def __str__(self):
        return self.email
```

### 2.4 更新设置文件

在项目的`settings.py`文件中，指定自定义用户模型：

```python
# myproject/settings.py

AUTH_USER_MODEL = 'accounts.CustomUser'
```

### 2.5 创建和应用迁移

在定义了自定义用户模型后，需要创建并应用数据库迁移：

```bash
python manage.py makemigrations accounts
python manage.py migrate
```

### 2.6 更新管理界面

为了让自定义用户模型在Django Admin中可用，需要在`accounts`应用的`admin.py`文件中注册它：

```python
from django.contrib import admin
from django.contrib.auth.admin import UserAdmin
from .models import CustomUser

admin.site.register(CustomUser, UserAdmin)
```

## 3. 实践练习

### 3.1 创建超级用户

使用以下命令创建一个超级用户：

```bash
python manage.py createsuperuser
```

按照提示输入用户名、电子邮件和密码。

### 3.2 登录到Django Admin

启动开发服务器：

```bash
python manage.py runserver
```

打开浏览器，访问`http://127.0.0.1:8000/admin/`，使用刚刚创建的超级用户登录。

### 3.3 添加自定义字段

在Django Admin中，尝试添加一个新的用户，并填写自定义字段（如电话号码和地址）。

## 4. 总结

通过本教程，你学会了如何在Django中自定义用户模型。自定义用户模型可以让你更灵活地管理用户信息，满足项目的特定需求。希望你能将这些知识应用到实际项目中，进一步提升你的Django开发技能。

## 5. 进一步学习

- **权限和组**：学习如何在自定义用户模型中管理权限和组。
- **用户认证系统**：深入了解Django的用户认证系统，包括登录、注销和注册视图。
- **模型关系**：探索如何在自定义用户模型中定义与其他模型的关系（如一对多、多对多关系）。

继续学习和实践，你将能够构建更复杂和功能丰富的Django应用！