---
title: Laravel 入门教程
date: 2023-10-05
description: 本课程将带你从零开始学习Laravel框架，掌握其基本概念和核心功能，包括路由、控制器、视图和数据库操作。
slug: laravel-beginners-guide
tags:
  - Laravel
  - 后端开发
  - PHP
category: 编程教程
keywords:
  - Laravel 入门
  - Laravel 教程
  - PHP 框架
---

# Laravel 入门教程

## 1. 简介

### 1.1 Laravel 是什么？
Laravel 是一个基于 PHP 的开源 Web 应用框架，由 Taylor Otwell 创建。它旨在通过提供优雅的语法和强大的功能，简化 Web 开发过程。Laravel 遵循 MVC（模型-视图-控制器）架构模式，使得开发者能够更高效地构建复杂的 Web 应用。

### 1.2 为什么选择 Laravel？
- **优雅的语法**：Laravel 提供了简洁、易读的语法，使得代码更易于维护。
- **丰富的功能**：内置了许多常用的功能，如路由、认证、缓存、队列等。
- **强大的社区支持**：Laravel 拥有庞大的社区和丰富的文档资源，便于学习和解决问题。

## 2. 环境搭建

### 2.1 安装 Composer
Composer 是 PHP 的依赖管理工具，Laravel 项目依赖于它。

```bash
curl -sS https://getcomposer.org/installer | php
sudo mv composer.phar /usr/local/bin/composer
```

### 2.2 安装 Laravel
使用 Composer 安装 Laravel 安装器。

```bash
composer global require laravel/installer
```

### 2.3 创建新项目
使用 Laravel 安装器创建一个新的 Laravel 项目。

```bash
laravel new my-first-laravel-app
```

### 2.4 启动开发服务器
进入项目目录并启动内置的开发服务器。

```bash
cd my-first-laravel-app
php artisan serve
```

访问 `http://localhost:8000`，你应该能看到 Laravel 的欢迎页面。

## 3. 基本概念

### 3.1 路由
路由是定义 URL 和处理请求的控制器方法之间的映射。

```php
// routes/web.php
Route::get('/', function () {
    return view('welcome');
});

Route::get('/hello', function () {
    return 'Hello, Laravel!';
});
```

### 3.2 控制器
控制器负责处理请求并返回响应。

```php
// app/Http/Controllers/HelloController.php
namespace App\Http\Controllers;

use Illuminate\Http\Request;

class HelloController extends Controller
{
    public function index()
    {
        return 'Hello from Controller!';
    }
}
```

在 `routes/web.php` 中注册控制器路由。

```php
Route::get('/hello-controller', [HelloController::class, 'index']);
```

### 3.3 视图
视图是用户界面的模板文件，通常使用 Blade 模板引擎。

```php
// resources/views/hello.blade.php
<html>
<body>
    <h1>Hello, {{ $name }}!</h1>
</body>
</html>
```

在控制器中传递数据到视图。

```php
public function index()
{
    return view('hello', ['name' => 'Laravel']);
}
```

### 3.4 模型
模型是与数据库交互的类，通常使用 Eloquent ORM。

```php
// app/Models/User.php
namespace App\Models;

use Illuminate\Database\Eloquent\Model;

class User extends Model
{
    protected $fillable = ['name', 'email', 'password'];
}
```

### 3.5 数据库迁移
迁移用于管理数据库表结构。

```php
// database/migrations/xxxx_xx_xx_create_users_table.php
use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\Schema;

class CreateUsersTable extends Migration
{
    public function up()
    {
        Schema::create('users', function (Blueprint $table) {
            $table->id();
            $table->string('name');
            $table->string('email')->unique();
            $table->string('password');
            $table->timestamps();
        });
    }

    public function down()
    {
        Schema::dropIfExists('users');
    }
}
```

运行迁移以创建表。

```bash
php artisan migrate
```

## 4. 实践练习

### 4.1 创建一个简单的博客系统
1. **创建数据库迁移**：创建 `posts` 表。
2. **创建模型**：创建 `Post` 模型。
3. **创建控制器**：创建 `PostController` 控制器。
4. **定义路由**：定义 `posts` 相关的路由。
5. **创建视图**：创建 `posts` 的列表和详情视图。

### 4.2 代码示例

#### 4.2.1 创建迁移

```php
// database/migrations/xxxx_xx_xx_create_posts_table.php
use Illuminate\Database\Migrations\Migration;
use Illuminate\Database\Schema\Blueprint;
use Illuminate\Support\Facades\Schema;

class CreatePostsTable extends Migration
{
    public function up()
    {
        Schema::create('posts', function (Blueprint $table) {
            $table->id();
            $table->string('title');
            $table->text('content');
            $table->timestamps();
        });
    }

    public function down()
    {
        Schema::dropIfExists('posts');
    }
}
```

#### 4.2.2 创建模型

```php
// app/Models/Post.php
namespace App\Models;

use Illuminate\Database\Eloquent\Model;

class Post extends Model
{
    protected $fillable = ['title', 'content'];
}
```

#### 4.2.3 创建控制器

```php
// app/Http/Controllers/PostController.php
namespace App\Http\Controllers;

use App\Models\Post;
use Illuminate\Http\Request;

class PostController extends Controller
{
    public function index()
    {
        $posts = Post::all();
        return view('posts.index', compact('posts'));
    }

    public function show($id)
    {
        $post = Post::findOrFail($id);
        return view('posts.show', compact('post'));
    }
}
```

#### 4.2.4 定义路由

```php
// routes/web.php
use App\Http\Controllers\PostController;

Route::get('/posts', [PostController::class, 'index']);
Route::get('/posts/{id}', [PostController::class, 'show']);
```

#### 4.2.5 创建视图

```php
// resources/views/posts/index.blade.php
<html>
<body>
    <h1>Posts</h1>
    <ul>
        @foreach ($posts as $post)
            <li><a href="/posts/{{ $post->id }}">{{ $post->title }}</a></li>
        @endforeach
    </ul>
</body>
</html>

// resources/views/posts/show.blade.php
<html>
<body>
    <h1>{{ $post->title }}</h1>
    <p>{{ $post->content }}</p>
</body>
</html>
```

## 5. 总结

通过本教程，你已经了解了 Laravel 的基本概念和使用方法，并完成了一个简单的博客系统。Laravel 提供了丰富的功能和优雅的语法，使得 Web 开发变得更加高效和愉快。继续探索 Laravel 的更多功能，如认证、缓存、队列等，将帮助你构建更复杂的应用。

## 6. 下一步

- **学习更多 Laravel 功能**：如认证、缓存、队列等。
- **深入理解 Eloquent ORM**：掌握更多数据库操作技巧。
- **实践项目**：尝试构建更复杂的应用，如电子商务网站或社交媒体应用。

希望本教程能帮助你顺利入门 Laravel，并在未来的开发中取得成功！