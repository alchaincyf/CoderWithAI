---
title: Docker 容器化入门教程
date: 2023-10-05
description: 本课程将带你深入了解Docker容器化的基本概念和实践操作，帮助你掌握如何使用Docker创建、部署和管理容器化应用。
slug: docker-containerization-tutorial
tags:
  - Docker
  - 容器化
  - DevOps
category: 编程技术
keywords:
  - Docker容器化
  - 容器技术
  - Docker教程
---

# Docker 容器化

## 1. 概述

### 1.1 什么是 Docker？
Docker 是一个开源的容器化平台，允许开发者将应用程序及其依赖项打包到一个轻量级、可移植的容器中。这些容器可以在任何支持 Docker 的环境中运行，确保应用程序在不同环境中的一致性。

### 1.2 为什么使用 Docker？
- **一致性**：确保应用程序在开发、测试和生产环境中行为一致。
- **可移植性**：容器可以在任何支持 Docker 的平台上运行。
- **隔离性**：容器之间相互隔离，避免依赖冲突。
- **快速部署**：容器启动速度快，便于快速部署和扩展。

## 2. Docker 基础

### 2.1 Docker 架构
Docker 由以下几个核心组件组成：
- **Docker 客户端**：用户与 Docker 交互的接口。
- **Docker 守护进程**：负责管理 Docker 容器和镜像。
- **Docker 镜像**：包含应用程序及其依赖的只读模板。
- **Docker 容器**：基于镜像运行的实例。
- **Docker Registry**：存储和分发 Docker 镜像的仓库。

### 2.2 Docker 安装
在不同操作系统上安装 Docker 的步骤有所不同。以下是一些常见操作系统的安装指南：

#### 2.2.1 Linux
```bash
# 更新包管理器
sudo apt-get update

# 安装 Docker
sudo apt-get install docker-ce docker-ce-cli containerd.io

# 启动 Docker 服务
sudo systemctl start docker

# 设置 Docker 开机自启
sudo systemctl enable docker
```

#### 2.2.2 macOS
下载并安装 [Docker Desktop for Mac](https://docs.docker.com/docker-for-mac/install/)。

#### 2.2.3 Windows
下载并安装 [Docker Desktop for Windows](https://docs.docker.com/docker-for-windows/install/)。

## 3. 创建 Docker 镜像

### 3.1 Dockerfile
Dockerfile 是一个文本文件，包含一系列指令，用于构建 Docker 镜像。以下是一个简单的 Dockerfile 示例：

```Dockerfile
# 使用官方 Node.js 镜像作为基础
FROM node:14

# 设置工作目录
WORKDIR /app

# 复制 package.json 和 package-lock.json
COPY package*.json ./

# 安装依赖
RUN npm install

# 复制应用程序代码
COPY . .

# 暴露端口
EXPOSE 4200

# 启动应用程序
CMD ["npm", "start"]
```

### 3.2 构建镜像
在包含 Dockerfile 的目录中运行以下命令来构建镜像：

```bash
docker build -t my-angular-app .
```

### 3.3 运行容器
使用以下命令运行容器：

```bash
docker run -p 4200:4200 my-angular-app
```

## 4. 实践练习

### 4.1 创建 Angular 应用
首先，创建一个简单的 Angular 应用：

```bash
ng new my-angular-app
cd my-angular-app
```

### 4.2 编写 Dockerfile
在项目根目录下创建一个 Dockerfile，内容如下：

```Dockerfile
FROM node:14
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .
EXPOSE 4200
CMD ["npm", "start"]
```

### 4.3 构建并运行容器
在项目根目录下运行以下命令：

```bash
docker build -t my-angular-app .
docker run -p 4200:4200 my-angular-app
```

打开浏览器访问 `http://localhost:4200`，你应该能看到 Angular 应用的欢迎页面。

## 5. 进阶主题

### 5.1 多阶段构建
多阶段构建可以减少最终镜像的大小，提高构建效率。以下是一个多阶段构建的示例：

```Dockerfile
# 第一阶段：构建应用
FROM node:14 as build
WORKDIR /app
COPY package*.json ./
RUN npm install
COPY . .
RUN npm run build

# 第二阶段：运行应用
FROM nginx:alpine
COPY --from=build /app/dist/my-angular-app /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
```

### 5.2 使用 Docker Compose
Docker Compose 是一个工具，用于定义和运行多容器 Docker 应用程序。以下是一个简单的 `docker-compose.yml` 示例：

```yaml
version: '3'
services:
  web:
    build: .
    ports:
      - "4200:4200"
```

运行以下命令启动应用：

```bash
docker-compose up
```

## 6. 总结

通过本教程，你已经学会了如何使用 Docker 容器化 Angular 应用。从安装 Docker 到创建和运行容器，再到多阶段构建和使用 Docker Compose，你已经掌握了 Docker 的基本和进阶用法。希望这些知识能帮助你在实际项目中更好地应用 Docker，提升开发和部署效率。