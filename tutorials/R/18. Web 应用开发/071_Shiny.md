---
title: 部署 Shiny 应用教程
date: 2023-10-05
description: 本教程详细介绍如何将R语言中的Shiny应用部署到不同的服务器环境，包括本地服务器、云平台和ShinyApps.io。
slug: deploy-shiny-app
tags:
  - R语言
  - Shiny
  - 应用部署
category: 编程教程
keywords:
  - Shiny应用
  - R语言部署
  - ShinyApps.io
---

# 部署 Shiny 应用

## 概述

Shiny 是 R 语言中用于创建交互式网页应用的一个强大工具。通过 Shiny，你可以将 R 代码转化为用户友好的网页应用，无需深入了解 HTML、CSS 或 JavaScript。本教程将详细介绍如何部署 Shiny 应用，使其能够在互联网上公开访问。

## 1. 安装必要的软件包

在开始之前，确保你已经安装了 `shiny` 和 `rsconnect` 包。`shiny` 包用于创建 Shiny 应用，而 `rsconnect` 包用于部署应用。

```R
install.packages("shiny")
install.packages("rsconnect")
```

## 2. 创建一个简单的 Shiny 应用

首先，我们创建一个简单的 Shiny 应用。以下是一个基本的 Shiny 应用代码示例：

```R
# app.R
library(shiny)

# UI 部分
ui <- fluidPage(
  titlePanel("Hello Shiny!"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 0, max = 100, value = 50)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# 服务器部分
server <- function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
}

# 运行应用
shinyApp(ui = ui, server = server)
```

将上述代码保存为 `app.R` 文件。

## 3. 在本地运行 Shiny 应用

在 RStudio 中打开 `app.R` 文件，然后点击“Run App”按钮。你的 Shiny 应用将在本地运行，并在默认浏览器中打开。

## 4. 部署 Shiny 应用

### 4.1 注册 ShinyApps.io 账户

ShinyApps.io 是 RStudio 提供的一个托管服务，用于部署 Shiny 应用。首先，访问 [ShinyApps.io](https://www.shinyapps.io/) 并注册一个免费账户。

### 4.2 配置 rsconnect

在 RStudio 中，运行以下代码来配置 `rsconnect` 包，以便与你的 ShinyApps.io 账户关联：

```R
library(rsconnect)
rsconnect::setAccountInfo(name='yourAccountName', token='yourToken', secret='yourSecret')
```

你需要从 ShinyApps.io 账户中获取 `name`、`token` 和 `secret` 信息。

### 4.3 部署应用

在 RStudio 中，打开 `app.R` 文件，然后点击“Publish”按钮。选择“ShinyApps.io”作为目标，然后点击“Publish”。

部署过程可能需要几分钟时间。完成后，你的 Shiny 应用将在 ShinyApps.io 上公开访问。

## 5. 实践练习

### 练习 1: 修改应用

修改 `app.R` 文件，添加一个新的输入控件（例如 `textInput`）和一个新的输出（例如 `textOutput`）。重新部署应用，并观察变化。

### 练习 2: 创建新应用

创建一个新的 Shiny 应用，使用 `ggplot2` 包生成一个交互式图表。部署该应用，并分享给你的朋友或同事。

## 6. 总结

通过本教程，你学会了如何创建一个简单的 Shiny 应用，并在 ShinyApps.io 上部署它。Shiny 提供了强大的工具，使你能够轻松地将 R 代码转化为交互式网页应用。继续探索 Shiny 的更多功能，创建更复杂的应用，并将其部署到互联网上。

## 7. 进一步学习

- 学习如何使用 `shinydashboard` 包创建更复杂的仪表板。
- 探索 Shiny 的更多高级功能，如 `reactive` 表达式和 `observe` 事件。
- 了解如何在企业环境中部署 Shiny 应用，使用 Docker 或 Kubernetes。

希望本教程对你有所帮助，祝你在 Shiny 应用开发中取得成功！