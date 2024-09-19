---
title: 国际化和本地化编程教程
date: 2023-10-05
description: 本课程详细讲解如何在编程中实现国际化和本地化，包括多语言支持、区域设置和本地化资源管理。
slug: internationalization-and-localization-programming
tags:
  - 国际化
  - 本地化
  - 编程
category: 编程技术
keywords:
  - 国际化编程
  - 本地化编程
  - 多语言支持
---

# 国际化和本地化

## 概述

国际化（Internationalization，简称 i18n）和本地化（Localization，简称 L10n）是构建多语言和多地区支持的 Web 应用的关键步骤。Django 提供了强大的工具来帮助开发者轻松实现这一目标。

## 国际化

国际化是指设计和开发应用程序，使其能够适应不同的语言和地区，而不需要进行工程上的更改。Django 通过以下方式支持国际化：

### 1. 语言设置

Django 允许你在项目中设置默认语言，并根据用户的需求动态更改语言。

```python
# settings.py
LANGUAGE_CODE = 'en-us'  # 默认语言为英语

LANGUAGES = [
    ('en', 'English'),
    ('es', 'Spanish'),
    ('zh-hans', 'Simplified Chinese'),
]
```

### 2. 翻译字符串

Django 使用 `gettext` 来标记需要翻译的字符串。你可以在模板和 Python 代码中使用 `{% trans %}` 和 `{% blocktrans %}` 标签。

```html
<!-- templates/base.html -->
<h1>{% trans "Welcome to our site" %}</h1>
```

在 Python 代码中：

```python
from django.utils.translation import gettext as _

def welcome_view(request):
    message = _("Welcome to our site")
    return HttpResponse(message)
```

### 3. 生成翻译文件

使用 `makemessages` 命令生成翻译文件：

```bash
django-admin makemessages -l es
```

这将生成一个 `.po` 文件，你可以在其中添加翻译。

### 4. 编译翻译

使用 `compilemessages` 命令编译翻译文件：

```bash
django-admin compilemessages
```

这将生成 `.mo` 文件，Django 会使用这些文件来提供翻译。

## 本地化

本地化是指根据特定地区的语言、文化和习惯调整应用程序的内容和功能。Django 通过以下方式支持本地化：

### 1. 时区设置

Django 允许你设置默认时区，并根据用户的需求动态更改时区。

```python
# settings.py
TIME_ZONE = 'UTC'  # 默认时区为 UTC

USE_I18N = True
USE_L10N = True
USE_TZ = True
```

### 2. 日期和时间格式

Django 会根据用户的语言和地区自动格式化日期和时间。

```html
<!-- templates/base.html -->
<p>{% now "SHORT_DATETIME_FORMAT" %}</p>
```

### 3. 数字格式

Django 会根据用户的语言和地区自动格式化数字。

```html
<!-- templates/base.html -->
<p>{{ 1234.56|floatformat:2 }}</p>
```

## 实践练习

### 练习 1：添加多语言支持

1. 在 `settings.py` 中设置 `LANGUAGES` 和 `LANGUAGE_CODE`。
2. 在模板中使用 `{% trans %}` 标签标记需要翻译的字符串。
3. 运行 `makemessages` 和 `compilemessages` 命令生成和编译翻译文件。

### 练习 2：动态更改语言

1. 创建一个视图，允许用户选择语言。
2. 使用 `django.utils.translation.activate` 动态更改语言。

```python
from django.utils.translation import activate

def set_language(request):
    lang = request.GET.get('lang')
    activate(lang)
    return redirect('home')
```

### 练习 3：本地化日期和时间

1. 在模板中使用 `{% now %}` 标签显示当前日期和时间。
2. 观察不同语言下的日期和时间格式变化。

## 总结

通过本教程，你学习了如何在 Django 中实现国际化和本地化。你了解了如何设置语言和时区，标记和翻译字符串，以及如何动态更改语言和格式化日期和时间。这些技能将帮助你构建适应不同语言和地区的 Web 应用程序。