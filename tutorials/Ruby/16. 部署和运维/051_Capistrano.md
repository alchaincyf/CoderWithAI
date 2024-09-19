---
title: Capistrano 部署教程
date: 2023-10-05
description: 本课程详细讲解如何使用Capistrano自动化部署Ruby on Rails应用，涵盖安装、配置、任务编写及常见问题解决。
slug: capistrano-deployment-tutorial
tags:
  - Capistrano
  - 部署
  - Ruby on Rails
category: 编程教程
keywords:
  - Capistrano 部署
  - Ruby on Rails 部署
  - 自动化部署
---

# Capistrano 部署教程

## 1. 概述

Capistrano 是一个用于自动化部署 Ruby 应用程序的工具。它通过 SSH 连接到远程服务器，执行一系列预定义的任务，从而实现应用程序的部署。Capistrano 支持多服务器部署，并且可以与各种版本控制系统（如 Git）集成。

### 1.1 为什么使用 Capistrano？

- **自动化部署**：减少手动操作，提高部署效率。
- **多服务器支持**：可以同时部署到多个服务器。
- **版本控制集成**：与 Git 等版本控制系统无缝集成。
- **可扩展性**：通过任务和钩子，可以自定义部署流程。

## 2. 安装 Capistrano

### 2.1 安装 Ruby

确保你已经安装了 Ruby。你可以通过以下命令检查 Ruby 版本：

```bash
ruby -v
```

如果没有安装 Ruby，可以使用以下命令安装：

```bash
# 使用 Homebrew 安装 Ruby (macOS)
brew install ruby

# 使用 RVM 安装 Ruby
\curl -sSL https://get.rvm.io | bash -s stable --ruby
```

### 2.2 安装 Capistrano

在项目目录下，使用 Bundler 安装 Capistrano：

```bash
# 创建 Gemfile
bundle init

# 编辑 Gemfile，添加 Capistrano
echo "gem 'capistrano'" >> Gemfile

# 安装依赖
bundle install
```

### 2.3 初始化 Capistrano

在项目根目录下运行以下命令，初始化 Capistrano：

```bash
bundle exec cap install
```

这将会生成以下文件和目录：

- `Capfile`：Capistrano 的主配置文件。
- `config/deploy.rb`：全局部署配置文件。
- `config/deploy/`：包含各个环境的部署配置文件（如 `production.rb` 和 `staging.rb`）。

## 3. 配置 Capistrano

### 3.1 配置 `deploy.rb`

打开 `config/deploy.rb` 文件，进行基本配置：

```ruby
# config/deploy.rb

# 设置应用程序名称
set :application, 'my_app'

# 设置 Git 仓库地址
set :repo_url, 'git@github.com:username/my_app.git'

# 设置部署路径
set :deploy_to, '/var/www/my_app'

# 设置分支
set :branch, 'main'

# 设置 SSH 用户
set :user, 'deploy'

# 设置 SSH 端口（可选）
set :port, 22

# 设置 SSH 密钥路径（可选）
set :ssh_options, {
  keys: %w(/home/deploy/.ssh/id_rsa),
  forward_agent: false,
  auth_methods: %w(publickey)
}
```

### 3.2 配置环境文件

在 `config/deploy/` 目录下，创建对应环境的配置文件，例如 `production.rb`：

```ruby
# config/deploy/production.rb

# 设置服务器
server 'example.com', user: 'deploy', roles: %w{app db web}

# 设置 SSH 端口（可选）
set :port, 22
```

## 4. 编写部署任务

Capistrano 允许你编写自定义任务来执行特定的部署操作。你可以在 `Capfile` 中定义任务，或者在 `lib/capistrano/tasks/` 目录下创建任务文件。

### 4.1 示例任务：重启应用服务器

创建一个任务来重启应用服务器：

```ruby
# lib/capistrano/tasks/restart.rake

namespace :deploy do
  desc 'Restart application'
  task :restart do
    on roles(:app), in: :sequence, wait: 5 do
      # 重启应用服务器的命令
      execute :sudo, :systemctl, :restart, :puma
    end
  end
end
```

### 4.2 调用任务

在 `Capfile` 中加载任务文件：

```ruby
# Capfile

# 加载自定义任务
Dir.glob('lib/capistrano/tasks/*.rake').each { |r| import r }
```

## 5. 执行部署

### 5.1 部署到生产环境

使用以下命令部署到生产环境：

```bash
bundle exec cap production deploy
```

### 5.2 部署到其他环境

你可以通过指定环境名称来部署到不同的环境，例如：

```bash
bundle exec cap staging deploy
```

## 6. 实践练习

### 6.1 创建一个简单的 Ruby 应用

创建一个简单的 Ruby 应用，并将其推送到 Git 仓库。

### 6.2 配置 Capistrano

按照教程配置 Capistrano，并设置好生产环境和测试环境的配置文件。

### 6.3 执行部署

尝试将应用部署到测试环境，并验证部署是否成功。

### 6.4 自定义任务

编写一个自定义任务，例如在部署完成后发送通知邮件。

## 7. 总结

通过本教程，你已经学会了如何使用 Capistrano 自动化部署 Ruby 应用程序。Capistrano 提供了强大的功能，可以帮助你简化部署流程，提高开发效率。继续探索 Capistrano 的更多高级功能，如多阶段部署、回滚机制等，进一步提升你的部署能力。

## 8. 进一步学习资源

- [Capistrano 官方文档](https://capistranorb.com/)
- [Ruby 官方网站](https://www.ruby-lang.org/)
- [Bundler 官方文档](https://bundler.io/)

通过不断实践和学习，你将能够熟练掌握 Capistrano 部署技术，并在实际项目中应用自如。祝你学习愉快！