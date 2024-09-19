---
title: 容器化技术入门：Docker基础教程
date: 2023-10-05
description: 本课程将带你深入了解Docker容器化技术，从基础概念到实际应用，帮助你掌握如何使用Docker构建、部署和管理容器化应用。
slug: docker-containerization-basics
tags:
  - Docker
  - 容器化
  - 云计算
category: 编程技术
keywords:
  - Docker教程
  - 容器化技术
  - Docker基础
---

# 容器化（Docker）

## 1. 什么是容器化？

容器化是一种轻量级的虚拟化技术，允许开发者将应用程序及其依赖项打包到一个独立的、可移植的容器中。Docker 是目前最流行的容器化平台，它使得应用程序可以在任何环境中以相同的方式运行。

### 1.1 为什么使用 Docker？

- **一致性**：确保应用程序在开发、测试和生产环境中运行一致。
- **隔离性**：每个容器都是独立的，不会影响其他容器或宿主机。
- **可移植性**：容器可以在任何支持 Docker 的环境中运行。
- **资源高效**：容器比虚拟机更轻量，启动更快，资源占用更少。

## 2. Docker 基础

### 2.1 Docker 架构

Docker 由以下几个核心组件组成：

- **Docker 客户端**：用户与 Docker 交互的接口，可以通过命令行或 API 调用。
- **Docker 守护进程**：运行在宿主机上，负责管理 Docker 容器、镜像、网络等。
- **Docker 镜像**：一个只读的模板，包含运行应用程序所需的所有文件和依赖项。
- **Docker 容器**：镜像的运行实例，可以启动、停止、删除等。
- **Docker 仓库**：存储和分发 Docker 镜像的地方，如 Docker Hub。

### 2.2 Docker 安装

在开始之前，你需要在你的机器上安装 Docker。以下是安装步骤：

#### 2.2.1 在 Ubuntu 上安装 Docker

```bash
# 更新包索引
sudo apt-get update

# 安装必要的包
sudo apt-get install apt-transport-https ca-certificates curl software-properties-common

# 添加 Docker 的官方 GPG 密钥
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo gpg --dearmor -o /usr/share/keyrings/docker-archive-keyring.gpg

# 添加 Docker 的稳定版仓库
echo "deb [arch=$(dpkg --print-architecture) signed-by=/usr/share/keyrings/docker-archive-keyring.gpg] https://download.docker.com/linux/ubuntu $(lsb_release -cs) stable" | sudo tee /etc/apt/sources.list.d/docker.list > /dev/null

# 更新包索引并安装 Docker
sudo apt-get update
sudo apt-get install docker-ce docker-ce-cli containerd.io

# 验证安装
sudo docker --version
```

#### 2.2.2 在 macOS 上安装 Docker

你可以通过 [Docker Desktop for Mac](https://www.docker.com/products/docker-desktop) 安装 Docker。

### 2.3 基本 Docker 命令

以下是一些常用的 Docker 命令：

```bash
# 查看 Docker 版本
docker --version

# 查看所有正在运行的容器
docker ps

# 查看所有容器（包括停止的）
docker ps -a

# 启动一个容器
docker run -d --name my_container nginx

# 停止一个容器
docker stop my_container

# 删除一个容器
docker rm my_container

# 查看所有镜像
docker images

# 删除一个镜像
docker rmi nginx
```

## 3. 创建第一个 Docker 容器

### 3.1 使用官方镜像

让我们使用官方的 Nginx 镜像来创建一个简单的 Web 服务器容器。

```bash
# 拉取 Nginx 镜像
docker pull nginx

# 运行 Nginx 容器
docker run -d --name my_nginx -p 8080:80 nginx
```

现在，打开浏览器并访问 `http://localhost:8080`，你应该会看到 Nginx 的欢迎页面。

### 3.2 自定义容器

你可以通过创建一个 `Dockerfile` 来自定义容器。以下是一个简单的 `Dockerfile` 示例：

```Dockerfile
# 使用官方 Node.js 镜像作为基础
FROM node:14

# 设置工作目录
WORKDIR /app

# 复制 package.json 和 package-lock.json
COPY package*.json ./

# 安装依赖
RUN npm install

# 复制应用代码
COPY . .

# 暴露端口
EXPOSE 3000

# 启动应用
CMD ["npm", "start"]
```

创建一个简单的 Node.js 应用：

```javascript
// app.js
const express = require('express');
const app = express();
const port = 3000;

app.get('/', (req, res) => {
  res.send('Hello, Docker!');
});

app.listen(port, () => {
  console.log(`App listening at http://localhost:${port}`);
});
```

在项目根目录下创建 `Dockerfile`，然后构建并运行容器：

```bash
# 构建镜像
docker build -t my_node_app .

# 运行容器
docker run -d --name my_node_app -p 3000:3000 my_node_app
```

访问 `http://localhost:3000`，你应该会看到 "Hello, Docker!"。

## 4. 实践练习

### 4.1 练习 1：创建一个简单的 Express 应用并容器化

1. 创建一个简单的 Express 应用。
2. 编写 `Dockerfile`。
3. 构建并运行容器。

### 4.2 练习 2：使用 Docker Compose 管理多容器应用

1. 创建一个包含 Node.js 应用和 MongoDB 的数据库应用。
2. 编写 `docker-compose.yml` 文件。
3. 使用 Docker Compose 启动应用。

## 5. 总结

通过本教程，你已经了解了 Docker 的基本概念、安装方法、常用命令以及如何创建和运行容器。容器化技术为应用程序的开发、测试和部署提供了极大的便利，是现代软件开发中不可或缺的一部分。

## 6. 进一步学习

- **Docker 网络**：学习如何配置 Docker 网络，实现容器间的通信。
- **Docker 卷**：了解如何使用 Docker 卷来持久化数据。
- **Docker Compose**：深入学习 Docker Compose，管理多容器应用。
- **CI/CD 与 Docker**：将 Docker 集成到 CI/CD 流程中，实现自动化部署。

通过不断实践和学习，你将能够更好地掌握 Docker，并将其应用于实际项目中。