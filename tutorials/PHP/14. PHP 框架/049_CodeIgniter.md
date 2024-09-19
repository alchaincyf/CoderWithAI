---
title: CodeIgniter 简介
date: 2023-10-05
description: 本课程将介绍CodeIgniter框架的基础知识，包括安装、配置、控制器、模型和视图的使用。
slug: codeigniter-introduction
tags:
  - PHP
  - Web开发
  - 框架
category: 编程教程
keywords:
  - CodeIgniter
  - PHP框架
  - Web开发
---

# CodeIgniter 简介

## 概述

CodeIgniter 是一个轻量级的 PHP 框架，旨在帮助开发者快速构建动态网站。它提供了丰富的库和工具，使得开发过程更加高效和便捷。CodeIgniter 的设计理念是简单、快速和灵活，适合初学者和有经验的开发者。

## 安装 CodeIgniter

### 环境要求

- PHP 版本 >= 5.6
- MySQL 或其他支持的数据库
- Web 服务器（如 Apache 或 Nginx）

### 安装步骤

1. **下载 CodeIgniter**

   访问 [CodeIgniter 官方网站](https://codeigniter.com/) 下载最新版本的 CodeIgniter。

2. **解压文件**

   将下载的压缩包解压到你的 Web 服务器的根目录（例如 `htdocs` 或 `www`）。

3. **配置数据库**

   打开 `application/config/database.php` 文件，配置数据库连接信息：

   ```php
   $db['default'] = array(
       'dsn'   => '',
       'hostname' => 'localhost',
       'username' => 'your_username',
       'password' => 'your_password',
       'database' => 'your_database',
       'dbdriver' => 'mysqli',
       'dbprefix' => '',
       'pconnect' => FALSE,
       'db_debug' => (ENVIRONMENT !== 'production'),
       'cache_on' => FALSE,
       'cachedir' => '',
       'char_set' => 'utf8',
       'dbcollat' => 'utf8_general_ci',
       'swap_pre' => '',
       'encrypt' => FALSE,
       'compress' => FALSE,
       'stricton' => FALSE,
       'failover' => array(),
       'save_queries' => TRUE
   );
   ```

4. **配置基本设置**

   打开 `application/config/config.php` 文件，配置基本设置，如网站 URL：

   ```php
   $config['base_url'] = 'http://your-domain.com/';
   ```

5. **运行项目**

   在浏览器中访问你的项目 URL，例如 `http://localhost/your-project-folder/`，你应该能看到 CodeIgniter 的欢迎页面。

## 基本概念

### MVC 架构

CodeIgniter 遵循 MVC（Model-View-Controller）架构模式：

- **Model**：处理数据逻辑，与数据库交互。
- **View**：负责显示数据，通常是 HTML 页面。
- **Controller**：处理用户请求，调用 Model 获取数据，并将数据传递给 View。

### 路由

CodeIgniter 使用路由来映射 URL 到特定的控制器和方法。默认情况下，URL 格式为 `http://example.com/controller/method/param`。

### 控制器

控制器是处理用户请求的核心部分。以下是一个简单的控制器示例：

```php
<?php
defined('BASEPATH') OR exit('No direct script access allowed');

class Welcome extends CI_Controller {

    public function index()
    {
        $this->load->view('welcome_message');
    }
}
```

### 视图

视图是用户看到的页面。以下是一个简单的视图示例：

```php
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <title>Welcome</title>
</head>
<body>
    <h1>Welcome to CodeIgniter!</h1>
</body>
</html>
```

### 模型

模型负责与数据库交互。以下是一个简单的模型示例：

```php
<?php
class User_model extends CI_Model {

    public function get_users()
    {
        $query = $this->db->get('users');
        return $query->result();
    }
}
```

## 实践练习

### 创建一个简单的博客系统

1. **创建数据库表**

   创建一个名为 `posts` 的表，包含以下字段：

   ```sql
   CREATE TABLE posts (
       id INT AUTO_INCREMENT PRIMARY KEY,
       title VARCHAR(255) NOT NULL,
       content TEXT NOT NULL,
       created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
   );
   ```

2. **创建模型**

   创建一个名为 `Post_model.php` 的模型文件：

   ```php
   <?php
   class Post_model extends CI_Model {

       public function get_posts()
       {
           $query = $this->db->get('posts');
           return $query->result();
       }

       public function insert_post($data)
       {
           return $this->db->insert('posts', $data);
       }
   }
   ```

3. **创建控制器**

   创建一个名为 `Blog.php` 的控制器文件：

   ```php
   <?php
   class Blog extends CI_Controller {

       public function __construct()
       {
           parent::__construct();
           $this->load->model('Post_model');
       }

       public function index()
       {
           $data['posts'] = $this->Post_model->get_posts();
           $this->load->view('blog_view', $data);
       }

       public function create()
       {
           $this->load->view('create_post_view');
       }

       public function store()
       {
           $data = array(
               'title' => $this->input->post('title'),
               'content' => $this->input->post('content')
           );

           $this->Post_model->insert_post($data);
           redirect('blog');
       }
   }
   ```

4. **创建视图**

   创建一个名为 `blog_view.php` 的视图文件：

   ```php
   <!DOCTYPE html>
   <html lang="en">
   <head>
       <meta charset="UTF-8">
       <title>Blog</title>
   </head>
   <body>
       <h1>Blog Posts</h1>
       <a href="<?php echo site_url('blog/create'); ?>">Create New Post</a>
       <ul>
           <?php foreach ($posts as $post): ?>
               <li>
                   <h2><?php echo $post->title; ?></h2>
                   <p><?php echo $post->content; ?></p>
               </li>
           <?php endforeach; ?>
       </ul>
   </body>
   </html>
   ```

   创建一个名为 `create_post_view.php` 的视图文件：

   ```php
   <!DOCTYPE html>
   <html lang="en">
   <head>
       <meta charset="UTF-8">
       <title>Create Post</title>
   </head>
   <body>
       <h1>Create New Post</h1>
       <form action="<?php echo site_url('blog/store'); ?>" method="post">
           <label for="title">Title:</label>
           <input type="text" name="title" id="title">
           <br>
           <label for="content">Content:</label>
           <textarea name="content" id="content"></textarea>
           <br>
           <button type="submit">Create Post</button>
       </form>
   </body>
   </html>
   ```

5. **测试应用**

   访问 `http://localhost/your-project-folder/blog`，你应该能看到博客文章列表。点击“Create New Post”按钮，填写表单并提交，新文章应该会显示在列表中。

## 总结

通过本教程，你已经了解了 CodeIgniter 的基本概念和使用方法。CodeIgniter 提供了强大的工具和库，帮助你快速构建动态网站。继续探索 CodeIgniter 的更多功能，如表单验证、文件上传、缓存等，进一步提升你的开发技能。

## 进一步学习

- **CodeIgniter 官方文档**：[https://codeigniter.com/user_guide/](https://codeigniter.com/user_guide/)
- **CodeIgniter 社区论坛**：[https://forum.codeigniter.com/](https://forum.codeigniter.com/)
- **CodeIgniter 扩展库**：[https://github.com/codeigniter4/CodeIgniter4](https://github.com/codeigniter4/CodeIgniter4)

希望这篇教程能帮助你快速入门 CodeIgniter，并在实际项目中应用所学知识。祝你编程愉快！