---
title: Passenger 和 Puma 服务器教程
date: 2023-10-05
description: 本课程详细介绍如何使用Passenger和Puma服务器来部署和管理Ruby on Rails应用程序，包括安装、配置和性能优化。
slug: passenger-puma-server-tutorial
tags:
  - Ruby on Rails
  - 服务器
  - 部署
category: 编程教程
keywords:
  - Passenger
  - Puma
  - Ruby on Rails部署
  - 服务器配置
  - 性能优化
---

# Passenger 和 Puma 服务器

## 概述

在本教程中，我们将深入探讨两个流行的 Ruby 服务器：Passenger 和 Puma。这两个服务器在 Ruby 社区中广泛使用，用于部署和运行 Ruby 应用程序。我们将从基础概念开始，逐步介绍它们的安装、配置和使用方法。

## 1. Passenger 服务器

### 1.1 什么是 Passenger？

Passenger 是一个用于 Ruby、Python 和 Node.js 应用程序的 Web 服务器和应用服务器。它支持多种部署方式，包括与 Apache 和 Nginx 集成。Passenger 以其易用性和高性能而闻名。

### 1.2 安装 Passenger

#### 1.2.1 使用 RubyGems 安装

你可以通过 RubyGems 安装 Passenger：

```bash
gem install passenger
```

#### 1.2.2 与 Apache 集成

如果你使用 Apache 作为 Web 服务器，可以按照以下步骤集成 Passenger：

1. 安装 Passenger 模块：

    ```bash
    passenger-install-apache2-module
    ```

2. 配置 Apache：

    编辑 Apache 配置文件（通常是 `/etc/apache2/httpd.conf`），添加以下内容：

    ```apache
    LoadModule passenger_module /usr/local/lib/ruby/gems/2.7.0/gems/passenger-6.0.6/buildout/apache2/mod_passenger.so
    <IfModule mod_passenger.c>
      PassengerRoot /usr/local/lib/ruby/gems/2.7.0/gems/passenger-6.0.6
      PassengerDefaultRuby /usr/local/bin/ruby
    </IfModule>
    ```

3. 重启 Apache：

    ```bash
    sudo apachectl restart
    ```

### 1.3 配置 Passenger

Passenger 的配置文件通常位于你的应用程序根目录下的 `config.ru` 文件中。你可以通过设置环境变量来调整 Passenger 的行为。

```ruby
# config.ru
run MyApp::Application
```

### 1.4 实践练习

1. 安装 Passenger 并将其与 Apache 集成。
2. 创建一个简单的 Ruby 应用程序，并使用 Passenger 部署它。

## 2. Puma 服务器

### 2.1 什么是 Puma？

Puma 是一个高性能的 Ruby Web 服务器，专为速度和并发性设计。它支持多线程和集群模式，适合处理高并发的 Web 应用程序。

### 2.2 安装 Puma

你可以通过 RubyGems 安装 Puma：

```bash
gem install puma
```

### 2.3 配置 Puma

Puma 的配置文件通常是一个 `puma.rb` 文件，位于你的应用程序根目录下。以下是一个简单的配置示例：

```ruby
# puma.rb
workers 2
threads 1, 6

app_dir = File.expand_path("../..", __FILE__)
shared_dir = "#{app_dir}/shared"

bind "unix://#{shared_dir}/sockets/puma.sock"

stdout_redirect "#{shared_dir}/log/puma.stdout.log", "#{shared_dir}/log/puma.stderr.log", true

pidfile "#{shared_dir}/pids/puma.pid"
state_path "#{shared_dir}/pids/puma.state"

activate_control_app
```

### 2.4 启动 Puma

你可以通过以下命令启动 Puma：

```bash
puma -C config/puma.rb
```

### 2.5 实践练习

1. 安装 Puma 并配置一个简单的 Ruby 应用程序。
2. 启动 Puma 并测试应用程序的性能。

## 3. 比较 Passenger 和 Puma

### 3.1 性能

- **Passenger**：适合与 Apache 或 Nginx 集成，性能稳定，易于配置。
- **Puma**：专为高并发设计，支持多线程和集群模式，性能优异。

### 3.2 易用性

- **Passenger**：提供多种部署方式，易于集成和配置。
- **Puma**：配置相对简单，但需要手动管理多线程和集群。

### 3.3 适用场景

- **Passenger**：适合需要稳定性和易用性的中小型应用。
- **Puma**：适合需要高并发处理的大型应用。

## 4. 总结

在本教程中，我们详细介绍了 Passenger 和 Puma 服务器的安装、配置和使用方法。通过实践练习，你应该能够熟练地部署和运行 Ruby 应用程序。选择合适的服务器取决于你的应用需求和性能要求。

## 5. 进一步学习

- 深入研究 Passenger 和 Puma 的高级配置选项。
- 探索其他 Ruby Web 服务器，如 Unicorn 和 Thin。
- 学习如何使用 Capistrano 自动化部署。

希望本教程对你有所帮助，祝你在 Ruby 编程的道路上越走越远！