---
title: 第一个 Django 应用教程
date: 2023-10-05
description: 本教程将引导您从零开始创建您的第一个 Django 应用，涵盖项目设置、模型创建、视图编写和模板使用等关键步骤。
slug: first-django-app
tags:
  - Django
  - Python
  - Web开发
category: 编程教程
keywords:
  - Django 教程
  - 第一个 Django 应用
  - Python Web开发
---

# 第一个 Django 应用

在本教程中，我们将逐步创建你的第一个 Django 应用。通过这个过程，你将学习到 Django 的基本概念和如何使用 Django 框架来构建一个简单的 Web 应用。我们将从环境设置开始，逐步深入到模型、视图和模板的使用。

## 1. Django 简介和特性

Django 是一个高级 Python Web 框架，它鼓励快速开发和干净、实用的设计。Django 遵循 MVT（Model-View-Template）架构，提供了许多内置功能，如用户认证、管理界面、表单处理等。

### 主要特性：
- **快速开发**：Django 提供了许多内置功能，减少了开发时间。
- **安全**：Django 内置了防止常见的 Web 攻击（如 CSRF、XSS 等）的功能。
- **可扩展**：Django 支持多种数据库，并且可以轻松扩展。
- **丰富的文档**：Django 有详细的官方文档和活跃的社区支持。

## 2. 安装和环境设置

在开始之前，你需要确保你的系统上已经安装了 Python 和 pip。然后，你可以使用 pip 来安装 Django。

```bash
pip install django
```

安装完成后，你可以通过以下命令验证 Django 是否安装成功：

```bash
django-admin --version
```

## 3. 项目结构和管理命令

### 创建项目

使用 `django-admin` 命令来创建一个新的 Django 项目：

```bash
django-admin startproject myfirstproject
```

这将在当前目录下创建一个名为 `myfirstproject` 的文件夹，包含以下文件和目录：

```
myfirstproject/
    manage.py
    myfirstproject/
        __init__.py
        settings.py
        urls.py
        asgi.py
        wsgi.py
```

### 项目结构说明

- **manage.py**：一个命令行实用程序，用于与 Django 项目进行交互。
- **myfirstproject/**：项目的配置目录。
  - **settings.py**：项目的配置文件。
  - **urls.py**：URL 配置文件。
  - **asgi.py** 和 **wsgi.py**：用于部署项目的 ASGI 和 WSGI 接口。

### 运行开发服务器

进入项目目录并运行开发服务器：

```bash
cd myfirstproject
python manage.py runserver
```

打开浏览器并访问 `http://127.0.0.1:8000/`，你应该会看到 Django 的欢迎页面。

## 4. MVT (Model-View-Template) 架构

Django 遵循 MVT 架构，这是一种设计模式，用于分离应用程序的不同部分：

- **Model**：负责数据存储和业务逻辑。
- **View**：处理用户请求并返回响应。
- **Template**：负责显示数据。

## 5. 第一个 Django 应用

### 创建应用

在 Django 中，项目可以包含多个应用。每个应用负责一个特定的功能。我们可以使用以下命令创建一个新的应用：

```bash
python manage.py startapp myfirstapp
```

这将在项目目录下创建一个名为 `myfirstapp` 的文件夹，包含以下文件和目录：

```
myfirstapp/
    __init__.py
    admin.py
    apps.py
    migrations/
        __init__.py
    models.py
    tests.py
    views.py
```

### 注册应用

在 `myfirstproject/settings.py` 文件中，找到 `INSTALLED_APPS` 列表，并添加你的应用：

```python
INSTALLED_APPS = [
    ...
    'myfirstapp',
]
```

### 定义视图

在 `myfirstapp/views.py` 文件中，定义一个简单的视图：

```python
from django.http import HttpResponse

def index(request):
    return HttpResponse("Hello, Django!")
```

### 配置 URL

在 `myfirstapp` 目录下创建一个 `urls.py` 文件，并添加以下内容：

```python
from django.urls import path
from . import views

urlpatterns = [
    path('', views.index, name='index'),
]
```

然后在项目的 `urls.py` 文件中包含这个应用的 URL 配置：

```python
from django.contrib import admin
from django.urls import include, path

urlpatterns = [
    path('admin/', admin.site.urls),
    path('', include('myfirstapp.urls')),
]
```

### 运行应用

现在，你可以重新启动开发服务器并访问 `http://127.0.0.1:8000/`，你应该会看到 "Hello, Django!" 的页面。

## 6. 模型定义和字段类型

### 定义模型

在 `myfirstapp/models.py` 文件中，定义一个简单的模型：

```python
from django.db import models

class Article(models.Model):
    title = models.CharField(max_length=200)
    content = models.TextField()
    pub_date = models.DateTimeField('date published')

    def __str__(self):
        return self.title
```

### 创建和应用迁移

在终端中运行以下命令来创建数据库迁移：

```bash
python manage.py makemigrations
```

然后应用迁移：

```bash
python manage.py migrate
```

### 注册模型到管理界面

在 `myfirstapp/admin.py` 文件中注册模型：

```python
from django.contrib import admin
from .models import Article

admin.site.register(Article)
```

### 创建超级用户

运行以下命令创建一个超级用户：

```bash
python manage.py createsuperuser
```

按照提示输入用户名、邮箱和密码。

### 访问管理界面

启动开发服务器并访问 `http://127.0.0.1:8000/admin/`，使用你创建的超级用户登录，你应该能够看到并管理 `Article` 模型。

## 7. 数据库迁移

数据库迁移是 Django 管理数据库模式变化的方式。每次你对模型进行更改时，都需要创建并应用迁移。

### 创建迁移

```bash
python manage.py makemigrations
```

### 应用迁移

```bash
python manage.py migrate
```

## 8. 查询集和管理器

### 查询集

查询集是 Django 从数据库中获取数据的方式。你可以在视图中使用查询集来获取数据。

```python
from .models import Article

def article_list(request):
    articles = Article.objects.all()
    return HttpResponse(articles)
```

### 管理器

管理器是 Django 模型用于数据库查询的接口。默认情况下，每个模型都有一个 `objects` 管理器。

## 9. 模型关系 (一对多、多对多)

### 一对多关系

在 `models.py` 中定义一对多关系：

```python
class Author(models.Model):
    name = models.CharField(max_length=100)

class Article(models.Model):
    title = models.CharField(max_length=200)
    content = models.TextField()
    pub_date = models.DateTimeField('date published')
    author = models.ForeignKey(Author, on_delete=models.CASCADE)
```

### 多对多关系

在 `models.py` 中定义多对多关系：

```python
class Tag(models.Model):
    name = models.CharField(max_length=50)

class Article(models.Model):
    title = models.CharField(max_length=200)
    content = models.TextField()
    pub_date = models.DateTimeField('date published')
    author = models.ForeignKey(Author, on_delete=models.CASCADE)
    tags = models.ManyToManyField(Tag)
```

## 10. 自定义模型方法

你可以在模型中定义自定义方法来封装业务逻辑。例如：

```python
class Article(models.Model):
    ...

    def is_recent(self):
        return self.pub_date >= timezone.now() - datetime.timedelta(days=7)
```

## 11. 函数视图和类视图

### 函数视图

函数视图是 Django 中最简单的视图形式。例如：

```python
def article_detail(request, article_id):
    article = get_object_or_404(Article, pk=article_id)
    return HttpResponse(article.content)
```

### 类视图

类视图提供了更多的灵活性和可重用性。例如：

```python
from django.views import View

class ArticleDetailView(View):
    def get(self, request, article_id):
        article = get_object_or_404(Article, pk=article_id)
        return HttpResponse(article.content)
```

## 12. URL 模式和命名

### URL 模式

在 `urls.py` 中定义 URL 模式：

```python
urlpatterns = [
    path('articles/', views.article_list, name='article_list'),
    path('articles/<int:article_id>/', views.article_detail, name='article_detail'),
]
```

### URL 命名

URL 命名允许你在模板和视图中引用 URL 而不用担心路径的变化。例如：

```html
<a href="{% url 'article_detail' article.id %}">Read more</a>
```

## 13. 视图参数和请求处理

### 视图参数

视图可以接受 URL 中的参数。例如：

```python
def article_detail(request, article_id):
    article = get_object_or_404(Article, pk=article_id)
    return HttpResponse(article.content)
```

### 请求处理

Django 的请求对象包含了所有关于当前请求的信息。你可以使用它来获取 GET 或 POST 参数。

```python
def search(request):
    query = request.GET.get('q')
    results = Article.objects.filter(title__icontains=query)
    return HttpResponse(results)
```

## 14. 通用视图

Django 提供了一些通用视图，可以大大简化常见任务的实现。例如：

```python
from django.views.generic import ListView, DetailView

class ArticleListView(ListView):
    model = Article
    template_name = 'article_list.html'

class ArticleDetailView(DetailView):
    model = Article
    template_name = 'article_detail.html'
```

## 15. 中间件

中间件是 Django 处理请求和响应的钩子。你可以在 `settings.py` 中配置中间件：

```python
MIDDLEWARE = [
    'django.middleware.security.SecurityMiddleware',
    'django.contrib.sessions.middleware.SessionMiddleware',
    'django.middleware.common.CommonMiddleware',
    'django.middleware.csrf.CsrfViewMiddleware',
    'django.contrib.auth.middleware.AuthenticationMiddleware',
    'django.contrib.messages.middleware.MessageMiddleware',
    'django.middleware.clickjacking.XFrameOptionsMiddleware',
]
```

## 16. 模板语法和标签

### 模板语法

Django 模板使用 `{{ }}` 来插入变量，使用 `{% %}` 来插入逻辑。例如：

```html
<h1>{{ article.title }}</h1>
<p>{{ article.content }}</p>
<p>Published on: {{ article.pub_date }}</p>
```

### 模板标签

Django 提供了许多内置模板标签，例如 `if`、`for` 等。例如：

```html
{% if article.is_recent %}
    <p>This article is recent.</p>
{% endif %}
```

## 17. 模板继承

模板继承允许你创建一个基础模板，并在其他模板中继承它。例如：

```html
<!-- base.html -->
<html>
<head>
    <title>{% block title %}My Site{% endblock %}</title>
</head>
<body>
    <div id="content">
        {% block content %}{% endblock %}
    </div>
</body>
</html>
```

```html
<!-- article_detail.html -->
{% extends "base.html" %}

{% block title %}{{ article.title }}{% endblock %}

{% block content %}
<h1>{{ article.title }}</h1>
<p>{{ article.content }}</p>
{% endblock %}
```

## 18. 上下文处理器

上下文处理器允许你在所有模板中添加变量。例如：

```python
# context_processors.py
def site_info(request):
    return {'site_name': 'My Django Site'}
```

在 `settings.py` 中配置上下文处理器：

```python
TEMPLATES = [
    {
        ...
        'OPTIONS': {
            'context_processors': [
                ...
                'myfirstapp.context_processors.site_info',
            ],
        },
    },
]
```

## 19. 自定义模板标签和过滤器

### 自定义模板标签

在 `myfirstapp` 目录下创建一个 `templatetags` 目录，并在其中创建一个 `custom_tags.py` 文件：

```python
from django import template

register = template.Library()

@register.simple_tag
def current_time(format_string):
    return datetime.datetime.now().strftime(format_string)
```

在模板中使用自定义标签：

```html
{% load custom_tags %}
<p>Current time: {% current_time "%Y-%m-%d %H:%M" %}</p>
```

### 自定义过滤器

在 `custom_tags.py` 中定义一个自定义过滤器：

```python
@register.filter
def lower(value):
    return value.lower()
```

在模板中使用自定义过滤器：

```html
<p>{{ article.title|lower }}</p>
```

## 20. 静态文件处理

### 配置静态文件

在 `settings.py` 中配置静态文件：

```python
STATIC_URL = '/static/'
STATICFILES_DIRS = [
    os.path.join(BASE_DIR, 'static'),
]
```

### 使用静态文件

在模板中使用静态文件：

```html
{% load static %}
<img src="{% static 'images/logo.png' %}" alt="Logo">
```

## 21. Django Form 类

### 创建表单

在 `myfirstapp` 目录下创建一个 `forms.py` 文件：

```python
from django import forms

class ArticleForm(forms.Form):
    title = forms.CharField(max_length=200)
    content = forms.CharField(widget=forms.Textarea)
```

### 在视图中使用表单

在视图中使用表单：

```python
from django.shortcuts import render
from .forms import ArticleForm

def create_article(request):
    if request.method == 'POST':
        form = ArticleForm(request.POST)
        if form.is_valid():
            # 处理表单数据
            pass
    else:
        form = ArticleForm()
    return render(request, 'create_article.html', {'form': form})
```

### 在模板中渲染表单

在模板中渲染表单：

```html
<form method="post">
    {% csrf_token %}
    {{ form.as_p }}
    <button type="submit">Submit</button>
</form>
```

## 22. 表单验证

Django 表单提供了内置的验证功能。例如：

```python
class ArticleForm(forms.Form):
    title = forms.CharField(max_length=200)
    content = forms.CharField(widget=forms.Textarea)

    def clean_title(self):
        title = self.cleaned_data['title']
        if len(title) < 5:
            raise forms.ValidationError("Title must be at least 5 characters long.")
        return title
```

## 23. ModelForm

`ModelForm` 是 Django 提供的一个便捷类，用于直接从模型生成表单。例如：

```python
from django.forms import ModelForm
from .models import Article

class ArticleForm(ModelForm):
    class Meta:
        model = Article
        fields = ['title', 'content']
```

## 24. 表单集 (Formsets)

表单集允许你在一个页面上处理多个表单。例如：

```python
from django.forms import formset_factory
from .forms import ArticleForm

ArticleFormSet = formset_factory(ArticleForm,