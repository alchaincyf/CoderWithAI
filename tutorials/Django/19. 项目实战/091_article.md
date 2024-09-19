---
title: 构建电子商务平台：从零到部署
date: 2023-10-05
description: 本课程将带你从零开始构建一个完整的电子商务平台，涵盖前端开发、后端逻辑、数据库设计以及最终的部署过程。
slug: ecommerce-platform-development
tags:
  - 电子商务
  - 平台开发
  - 全栈开发
category: 编程教程
keywords:
  - 电子商务平台
  - 前端开发
  - 后端开发
---

# Django 电子商务平台开发教程

## 1. Django 简介和特性

Django 是一个高级 Python Web 框架，鼓励快速开发和简洁、实用的设计。它遵循 MVT（Model-View-Template）架构，提供了许多内置功能，如用户认证、内容管理、表单处理、文件上传等。

### 1.1 Django 的主要特性
- **ORM（对象关系映射）**：简化数据库操作。
- **Admin 后台**：自动生成管理界面。
- **表单处理**：简化表单创建和验证。
- **安全特性**：内置 CSRF 保护、XSS 防御等。

## 2. 安装和环境设置

### 2.1 安装 Django
```bash
pip install django
```

### 2.2 创建项目
```bash
django-admin startproject ecommerce_platform
```

### 2.3 运行开发服务器
```bash
cd ecommerce_platform
python manage.py runserver
```

## 3. 项目结构和管理命令

### 3.1 项目结构
```
ecommerce_platform/
    manage.py
    ecommerce_platform/
        __init__.py
        settings.py
        urls.py
        asgi.py
        wsgi.py
```

### 3.2 常用管理命令
- **创建应用**：`python manage.py startapp products`
- **运行服务器**：`python manage.py runserver`
- **创建数据库迁移**：`python manage.py makemigrations`
- **应用数据库迁移**：`python manage.py migrate`

## 4. MVT 架构

### 4.1 Model
模型是数据的唯一且明确的信息源。它包含数据的基本字段和行为。

### 4.2 View
视图是处理请求并返回响应的逻辑。它可以是函数或类。

### 4.3 Template
模板是用于生成 HTML 的文本文件。它包含变量和标签。

## 5. 第一个 Django 应用

### 5.1 创建应用
```bash
python manage.py startapp products
```

### 5.2 定义模型
```python
# products/models.py
from django.db import models

class Product(models.Model):
    name = models.CharField(max_length=255)
    price = models.DecimalField(max_digits=10, decimal_places=2)
    description = models.TextField()
```

### 5.3 创建和应用迁移
```bash
python manage.py makemigrations
python manage.py migrate
```

## 6. 模型定义和字段类型

### 6.1 常用字段类型
- `CharField`：用于短字符串。
- `TextField`：用于长文本。
- `IntegerField`：用于整数。
- `DecimalField`：用于浮点数。
- `DateTimeField`：用于日期和时间。

### 6.2 自定义模型方法
```python
class Product(models.Model):
    # ...

    def is_expensive(self):
        return self.price > 1000
```

## 7. 数据库迁移

### 7.1 创建迁移
```bash
python manage.py makemigrations
```

### 7.2 应用迁移
```bash
python manage.py migrate
```

## 8. 查询集和管理器

### 8.1 查询集
查询集是数据库查询的集合。
```python
products = Product.objects.all()
expensive_products = Product.objects.filter(price__gt=1000)
```

### 8.2 管理器
管理器是用于创建查询集的对象。
```python
class ProductManager(models.Manager):
    def expensive_products(self):
        return self.filter(price__gt=1000)

class Product(models.Model):
    # ...
    objects = ProductManager()
```

## 9. 模型关系

### 9.1 一对多关系
```python
class Category(models.Model):
    name = models.CharField(max_length=255)

class Product(models.Model):
    category = models.ForeignKey(Category, on_delete=models.CASCADE)
    # ...
```

### 9.2 多对多关系
```python
class Tag(models.Model):
    name = models.CharField(max_length=255)

class Product(models.Model):
    tags = models.ManyToManyField(Tag)
    # ...
```

## 10. 自定义模型方法

```python
class Product(models.Model):
    # ...

    def is_expensive(self):
        return self.price > 1000
```

## 11. 函数视图和类视图

### 11.1 函数视图
```python
# products/views.py
from django.http import HttpResponse

def product_list(request):
    return HttpResponse("Product List")
```

### 11.2 类视图
```python
from django.views import View

class ProductListView(View):
    def get(self, request):
        return HttpResponse("Product List")
```

## 12. URL 模式和命名

### 12.1 URL 模式
```python
# products/urls.py
from django.urls import path
from . import views

urlpatterns = [
    path('products/', views.product_list, name='product_list'),
]
```

### 12.2 命名 URL
```html
<a href="{% url 'product_list' %}">Product List</a>
```

## 13. 视图参数和请求处理

### 13.1 视图参数
```python
def product_detail(request, product_id):
    return HttpResponse(f"Product {product_id}")
```

### 13.2 请求处理
```python
def product_detail(request, product_id):
    if request.method == 'GET':
        return HttpResponse(f"Product {product_id}")
    elif request.method == 'POST':
        # Handle POST request
        pass
```

## 14. 通用视图

### 14.1 列表视图
```python
from django.views.generic import ListView
from .models import Product

class ProductListView(ListView):
    model = Product
    template_name = 'products/product_list.html'
```

### 14.2 详情视图
```python
from django.views.generic import DetailView
from .models import Product

class ProductDetailView(DetailView):
    model = Product
    template_name = 'products/product_detail.html'
```

## 15. 中间件

### 15.1 自定义中间件
```python
# middleware.py
class SimpleMiddleware:
    def __init__(self, get_response):
        self.get_response = get_response

    def __call__(self, request):
        # Code to be executed for each request before
        # the view (and later middleware) are called.
        response = self.get_response(request)
        # Code to be executed for each request/response after
        # the view is called.
        return response
```

## 16. 模板语法和标签

### 16.1 模板语法
```html
<h1>{{ product.name }}</h1>
<p>{{ product.description }}</p>
```

### 16.2 模板标签
```html
{% if product.is_expensive %}
    <p>Expensive</p>
{% else %}
    <p>Affordable</p>
{% endif %}
```

## 17. 模板继承

### 17.1 基础模板
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

### 17.2 继承模板
```html
<!-- product_list.html -->
{% extends "base.html" %}

{% block title %}Product List{% endblock %}

{% block content %}
    <h1>Product List</h1>
    <ul>
        {% for product in products %}
            <li>{{ product.name }}</li>
        {% endfor %}
    </ul>
{% endblock %}
```

## 18. 上下文处理器

### 18.1 自定义上下文处理器
```python
# context_processors.py
def cart_items(request):
    return {'cart_items': request.session.get('cart', [])}
```

### 18.2 注册上下文处理器
```python
# settings.py
TEMPLATES = [
    {
        # ...
        'OPTIONS': {
            'context_processors': [
                # ...
                'ecommerce_platform.context_processors.cart_items',
            ],
        },
    },
]
```

## 19. 自定义模板标签和过滤器

### 19.1 自定义过滤器
```python
# templatetags/custom_filters.py
from django import template

register = template.Library()

@register.filter
def currency(value):
    return f"${value:.2f}"
```

### 19.2 使用过滤器
```html
{{ product.price|currency }}
```

## 20. 静态文件处理

### 20.1 配置静态文件
```python
# settings.py
STATIC_URL = '/static/'
STATICFILES_DIRS = [
    BASE_DIR / "static",
]
```

### 20.2 使用静态文件
```html
{% load static %}
<link rel="stylesheet" href="{% static 'css/styles.css' %}">
```

## 21. Django Form 类

### 21.1 创建表单
```python
# forms.py
from django import forms

class ProductForm(forms.Form):
    name = forms.CharField(max_length=255)
    price = forms.DecimalField(max_digits=10, decimal_places=2)
    description = forms.CharField(widget=forms.Textarea)
```

### 21.2 使用表单
```python
# views.py
from django.shortcuts import render
from .forms import ProductForm

def add_product(request):
    if request.method == 'POST':
        form = ProductForm(request.POST)
        if form.is_valid():
            # Process form data
            pass
    else:
        form = ProductForm()
    return render(request, 'products/add_product.html', {'form': form})
```

## 22. 表单验证

### 22.1 自定义验证
```python
class ProductForm(forms.Form):
    # ...

    def clean_price(self):
        price = self.cleaned_data['price']
        if price < 0:
            raise forms.ValidationError("Price cannot be negative")
        return price
```

## 23. ModelForm

### 23.1 创建 ModelForm
```python
# forms.py
from django.forms import ModelForm
from .models import Product

class ProductForm(ModelForm):
    class Meta:
        model = Product
        fields = ['name', 'price', 'description']
```

### 23.2 使用 ModelForm
```python
# views.py
from django.shortcuts import render, redirect
from .forms import ProductForm

def add_product(request):
    if request.method == 'POST':
        form = ProductForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect('product_list')
    else:
        form = ProductForm()
    return render(request, 'products/add_product.html', {'form': form})
```

## 24. 表单集 (Formsets)

### 24.1 创建表单集
```python
# forms.py
from django.forms import modelformset_factory
from .models import Product

ProductFormSet = modelformset_factory(Product, fields=('name', 'price'), extra=2)
```

### 24.2 使用表单集
```python
# views.py
from django.shortcuts import render
from .forms import ProductFormSet

def manage_products(request):
    formset = ProductFormSet(request.POST or None)
    if request.method == 'POST' and formset.is_valid():
        formset.save()
    return render(request, 'products/manage_products.html', {'formset': formset})
```

## 25. 文件上传处理

### 25.1 配置文件上传
```python
# settings.py
MEDIA_URL = '/media/'
MEDIA_ROOT = BASE_DIR / "media"
```

### 25.2 处理文件上传
```python
# models.py
class Product(models.Model):
    # ...
    image = models.ImageField(upload_to='products/')
```

## 26. 用户认证系统

### 26.1 用户注册
```python
# views.py
from django.contrib.auth.forms import UserCreationForm
from django.shortcuts import render, redirect

def register(request):
    if request.method == 'POST':
        form = UserCreationForm(request.POST)
        if form.is_valid():
            form.save()
            return redirect('login')
    else:
        form = UserCreationForm()
    return render(request, 'registration/register.html', {'form': form})
```

### 26.2 用户登录
```python
# views.py
from django.contrib.auth import authenticate, login
from django.shortcuts import render, redirect

def user_login(request):
    if request.method == 'POST':
        username = request.POST['username']
        password = request.POST['password']
        user = authenticate(request, username=username, password=password)
        if user is not None:
            login(request, user)
            return redirect('home')
    return render(request, 'registration/login.html')
```

## 27. 自定义用户模型

### 27.1 创建自定义用户模型
```python
# models.py
from django.contrib.auth.models import AbstractUser

class CustomUser(AbstractUser):
    # Add custom fields
    pass
```

### 27.2 配置自定义用户模型
```python
# settings.py
AUTH_USER_MODEL = 'accounts.CustomUser'
```

## 28. 权限和组

### 28.1 创建权限
```python
from django.contrib.auth.models import Permission
from django.contrib.contenttypes.models import ContentType
from .models import Product

content_type = ContentType.objects.get_for_model(Product)
permission = Permission.objects.create(
    codename='can_publish',
    name='Can Publish Products',
    content_type=content_type,
)
```

### 28.2 分配权限
```python
from django.contrib.auth.models import Group

group = Group.objects.create(name='Publishers')
group.permissions.add(permission)
```

## 29. 登录、注销和注册视图

### 29.1 登录视图
```python
from django.contrib.auth.views import LoginView

urlpatterns = [
    path('login/', LoginView.as_view(), name='login'),
]
```

### 29.2 注销视图
```python
from django.contrib.auth.views import LogoutView

urlpatterns = [
    path('logout/', LogoutView.as_view(), name='logout'),
]
```

### 29.3 注册视图
```python
from django.contrib.auth.forms import UserCreationForm
from django.views.generic.edit import CreateView

class RegisterView(CreateView):
    form_class = UserCreationForm
    template_name = 'registration/register.html'
    success_url = '/login/'
```

## 30. 基于类的权限控制

### 30.1 权限检查
```python
from django.contrib.auth.mixins import LoginRequiredMixin
from django.views.generic import View

class ProtectedView(LoginRequiredMixin, View):
    def get(self, request):
        return HttpResponse("Protected View")
```

## 31. Admin 站点定制

### 31.1 模型注册
```python
# admin.py
from django.contrib import admin
from .models import Product

admin.site.register(Product)
```

### 31.2 