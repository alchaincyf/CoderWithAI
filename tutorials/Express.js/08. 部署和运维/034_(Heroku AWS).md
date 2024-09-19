---
title: 部署到云平台 (Heroku, AWS) 教程
date: 2023-10-05
description: 本课程将教你如何将你的应用程序部署到流行的云平台如Heroku和AWS，涵盖从基础设置到高级配置的完整流程。
slug: deploy-to-cloud-platforms
tags:
  - 云部署
  - Heroku
  - AWS
category: 云服务与部署
keywords:
  - 云平台部署
  - Heroku教程
  - AWS部署
---

# 部署到云平台 (Heroku, AWS)

## 1. 概述

在现代Web开发中，将应用程序部署到云平台是至关重要的一步。云平台提供了可扩展的基础设施，使得开发者能够轻松地将应用部署到全球用户。本教程将详细介绍如何将Express.js应用部署到两个流行的云平台：Heroku和AWS。

## 2. Heroku 部署

### 2.1 Heroku 简介

Heroku 是一个支持多种编程语言的云平台即服务（PaaS）。它简化了应用的部署和管理过程，特别适合初学者和快速原型开发。

### 2.2 准备工作

在开始之前，确保你已经安装了以下工具：

- Node.js 和 npm
- Git
- Heroku CLI

### 2.3 创建 Heroku 应用

1. **登录 Heroku**

   打开终端并运行：

   ```bash
   heroku login
   ```

   按照提示完成登录。

2. **创建 Heroku 应用**

   在项目目录下运行：

   ```bash
   heroku create
   ```

   这将创建一个新的Heroku应用，并返回一个随机的应用名称。

### 2.4 配置 `package.json`

确保你的 `package.json` 文件中包含以下内容：

```json
{
  "name": "your-app-name",
  "version": "1.0.0",
  "scripts": {
    "start": "node app.js"
  },
  "dependencies": {
    "express": "^4.17.1"
  }
}
```

### 2.5 部署应用

1. **初始化 Git 仓库**

   如果你还没有初始化 Git 仓库，运行：

   ```bash
   git init
   git add .
   git commit -m "Initial commit"
   ```

2. **添加 Heroku 远程仓库**

   运行：

   ```bash
   heroku git:remote -a your-app-name
   ```

3. **部署代码**

   运行：

   ```bash
   git push heroku master
   ```

   部署完成后，Heroku 会提供一个URL，你可以通过该URL访问你的应用。

### 2.6 查看日志

如果应用没有正常运行，可以通过以下命令查看日志：

```bash
heroku logs --tail
```

## 3. AWS 部署

### 3.1 AWS 简介

Amazon Web Services (AWS) 是一个全面的云服务平台，提供多种服务，包括计算、存储、数据库、分析等。AWS 提供了更高的灵活性和控制力，适合需要定制化解决方案的应用。

### 3.2 准备工作

在开始之前，确保你已经：

- 创建了一个 AWS 账户
- 安装了 AWS CLI
- 配置了 AWS CLI 凭据

### 3.3 创建 EC2 实例

1. **登录 AWS 管理控制台**

   访问 [AWS 管理控制台](https://aws.amazon.com/console/) 并登录。

2. **启动 EC2 实例**

   在 EC2 控制台中，点击“启动实例”。选择一个合适的 Amazon 机器映像（AMI），例如 Amazon Linux 2。

3. **配置实例**

   选择实例类型，配置安全组（确保开放必要的端口，如 80 和 443），并创建一个密钥对。

4. **启动实例**

   启动实例后，记下公有 IP 地址。

### 3.4 配置 EC2 实例

1. **连接到实例**

   使用 SSH 连接到你的 EC2 实例：

   ```bash
   ssh -i your-key.pem ec2-user@your-instance-ip
   ```

2. **安装 Node.js 和 npm**

   在实例上运行：

   ```bash
   sudo yum install -y nodejs
   ```

3. **安装 Git**

   运行：

   ```bash
   sudo yum install -y git
   ```

4. **克隆你的项目**

   运行：

   ```bash
   git clone your-repo-url
   cd your-repo
   ```

5. **安装依赖**

   运行：

   ```bash
   npm install
   ```

6. **启动应用**

   运行：

   ```bash
   node app.js
   ```

### 3.5 配置域名和 SSL

1. **购买域名**

   在 AWS Route 53 中购买一个域名。

2. **配置 DNS**

   将域名指向你的 EC2 实例的公有 IP 地址。

3. **安装和配置 SSL**

   使用 Certbot 获取免费的 SSL 证书：

   ```bash
   sudo yum install -y certbot
   sudo certbot --nginx
   ```

### 3.6 监控和管理

AWS 提供了多种监控和管理工具，如 CloudWatch 和 CloudTrail，帮助你监控应用的性能和安全性。

## 4. 实践练习

### 4.1 练习 1：Heroku 部署

1. 创建一个新的 Express.js 应用。
2. 使用 Heroku CLI 部署该应用。
3. 查看应用的日志，确保其正常运行。

### 4.2 练习 2：AWS 部署

1. 创建一个新的 EC2 实例。
2. 在实例上部署你的 Express.js 应用。
3. 配置域名和 SSL，确保应用可以通过 HTTPS 访问。

## 5. 总结

通过本教程，你学习了如何将 Express.js 应用部署到 Heroku 和 AWS。Heroku 提供了简单易用的部署体验，适合快速开发和原型验证。AWS 则提供了更高的灵活性和控制力，适合需要定制化解决方案的应用。希望你能通过这些实践练习，掌握云平台部署的基本技能。