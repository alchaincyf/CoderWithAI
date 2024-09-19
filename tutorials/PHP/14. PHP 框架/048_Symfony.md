---
title: Symfony 基础教程
date: 2023-10-05
description: 本课程将带你深入了解Symfony框架的基础知识，包括安装、配置、路由、控制器和模板等核心概念。
slug: symfony-basics
tags:
  - Symfony
  - PHP
  - Web开发
category: 编程教程
keywords:
  - Symfony基础
  - Symfony教程
  - PHP框架
---

# Symfony 基础教程

## 1. 概述

Symfony 是一个用于构建高性能 PHP 应用程序的框架。它遵循 MVC（模型-视图-控制器）架构模式，提供了丰富的工具和组件，帮助开发者快速构建可维护和可扩展的应用程序。

### 1.1 Symfony 的历史

Symfony 最初由 Fabien Potencier 于 2005 年创建，目的是为了提供一个灵活且可扩展的 PHP 框架。随着时间的推移，Symfony 逐渐成为 PHP 社区中最受欢迎的框架之一，被广泛应用于各种类型的项目中。

### 1.2 为什么选择 Symfony？

- **模块化**：Symfony 提供了许多独立的组件，开发者可以根据需要选择使用。
- **可扩展性**：Symfony 的设计允许开发者轻松扩展和定制框架。
- **文档丰富**：Symfony 拥有详细的官方文档和活跃的社区支持。
- **性能优化**：Symfony 经过优化，能够处理高流量的应用程序。

## 2. 环境搭建

在开始使用 Symfony 之前，我们需要搭建一个适合的开发环境。Symfony 推荐使用 Composer 来管理依赖和项目结构。

### 2.1 安装 Composer

Composer 是 PHP 的依赖管理工具。首先，你需要在系统上安装 Composer。

```bash
curl -sS https://getcomposer.org/installer | php
mv composer.phar /usr/local/bin/composer
```

### 2.2 安装 Symfony CLI

Symfony CLI 是一个命令行工具，用于管理 Symfony 项目。你可以通过以下命令安装：

```bash
wget https://get.symfony.com/cli/installer -O - | bash
```

### 2.3 创建 Symfony 项目

使用 Symfony CLI 创建一个新的 Symfony 项目：

```bash
symfony new my_project_name
```

这将创建一个名为 `my_project_name` 的新项目，并自动安装所有必要的依赖。

## 3. Symfony 项目结构

Symfony 项目的目录结构如下：

```
my_project_name/
├── bin/
├── config/
├── public/
├── src/
├── templates/
├── tests/
├── var/
└── vendor/
```

- **bin/**：包含 Symfony 的命令行工具。
- **config/**：包含应用程序的配置文件。
- **public/**：包含公共文件，如 CSS、JavaScript 和图像。
- **src/**：包含应用程序的源代码。
- **templates/**：包含 Twig 模板文件。
- **tests/**：包含测试代码。
- **var/**：包含缓存、日志和其他临时文件。
- **vendor/**：包含 Composer 安装的依赖项。

## 4. 控制器和路由

在 Symfony 中，控制器负责处理 HTTP 请求并返回响应。路由定义了 URL 如何映射到控制器。

### 4.1 创建控制器

在 `src/Controller/` 目录下创建一个新的控制器文件 `DefaultController.php`：

```php
<?php

namespace App\Controller;

use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class DefaultController
{
    /**
     * @Route("/", name="homepage")
     */
    public function index(): Response
    {
        return new Response(
            '<html><body>Welcome to Symfony!</body></html>'
        );
    }
}
```

### 4.2 定义路由

在上面的代码中，`@Route("/", name="homepage")` 定义了一个路由，将根 URL `/` 映射到 `index` 方法。

### 4.3 运行项目

使用 Symfony CLI 启动内置的 Web 服务器：

```bash
symfony serve
```

打开浏览器并访问 `http://localhost:8000`，你应该会看到 "Welcome to Symfony!" 的消息。

## 5. 模板引擎

Symfony 使用 Twig 作为默认的模板引擎。Twig 是一个灵活且功能强大的模板引擎，支持模板继承、过滤器和扩展。

### 5.1 创建模板

在 `templates/` 目录下创建一个新的模板文件 `base.html.twig`：

```twig
<!DOCTYPE html>
<html>
    <head>
        <title>{% block title %}Welcome!{% endblock %}</title>
    </head>
    <body>
        {% block body %}{% endblock %}
    </body>
</html>
```

### 5.2 使用模板

在 `DefaultController` 中使用 Twig 模板：

```php
use Symfony\Bundle\FrameworkBundle\Controller\AbstractController;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class DefaultController extends AbstractController
{
    /**
     * @Route("/", name="homepage")
     */
    public function index(): Response
    {
        return $this->render('default/index.html.twig', [
            'title' => 'Welcome to Symfony!',
        ]);
    }
}
```

在 `templates/default/` 目录下创建 `index.html.twig` 文件：

```twig
{% extends 'base.html.twig' %}

{% block title %}{{ title }}{% endblock %}

{% block body %}
    <h1>{{ title }}</h1>
{% endblock %}
```

## 6. 数据库和 ORM

Symfony 使用 Doctrine ORM 来管理数据库。Doctrine 是一个强大的 ORM 工具，支持多种数据库系统。

### 6.1 配置数据库

在 `config/packages/doctrine.yaml` 中配置数据库连接：

```yaml
doctrine:
    dbal:
        url: '%env(resolve:DATABASE_URL)%'
```

在 `.env` 文件中设置数据库连接字符串：

```
DATABASE_URL="mysql://db_user:db_password@127.0.0.1:3306/db_name"
```

### 6.2 创建实体

使用 Doctrine 创建一个实体类 `Product`：

```bash
php bin/console make:entity Product
```

按照提示添加字段，例如 `name` 和 `price`。

### 6.3 生成数据库表

生成数据库表并应用迁移：

```bash
php bin/console doctrine:migrations:diff
php bin/console doctrine:migrations:migrate
```

### 6.4 使用实体

在控制器中使用 `Product` 实体：

```php
use App\Entity\Product;
use Doctrine\ORM\EntityManagerInterface;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class ProductController extends AbstractController
{
    /**
     * @Route("/product/create", name="product_create")
     */
    public function createProduct(EntityManagerInterface $entityManager): Response
    {
        $product = new Product();
        $product->setName('Keyboard');
        $product->setPrice(19.99);

        $entityManager->persist($product);
        $entityManager->flush();

        return new Response('Saved new product with id '.$product->getId());
    }
}
```

## 7. 实践练习

### 7.1 练习 1：创建用户注册页面

1. 创建一个新的控制器 `RegistrationController`。
2. 创建一个表单类 `RegistrationFormType`。
3. 在模板中渲染表单并处理提交。

### 7.2 练习 2：实现产品列表页面

1. 创建一个新的控制器 `ProductListController`。
2. 查询数据库中的所有产品。
3. 在模板中显示产品列表。

## 8. 总结

通过本教程，你已经学习了 Symfony 的基础知识，包括环境搭建、项目结构、控制器和路由、模板引擎、数据库和 ORM。Symfony 是一个功能强大的框架，适合构建各种类型的 PHP 应用程序。继续探索 Symfony 的更多功能，并尝试在实际项目中应用这些知识。

## 9. 进一步学习

- **Symfony 官方文档**：https://symfony.com/doc/current/index.html
- **Doctrine ORM 文档**：https://www.doctrine-project.org/projects/doctrine-orm/en/current/index.html
- **Twig 模板引擎文档**：https://twig.symfony.com/doc/3.x/

通过不断学习和实践，你将能够掌握 Symfony 的更多高级功能，并构建出更加复杂和强大的应用程序。