---
title: 描述性统计基础教程
date: 2023-10-05
description: 本课程介绍描述性统计的基本概念和应用，包括数据集中趋势、离散程度和分布形态的测量方法。
slug: descriptive-statistics-basics
tags:
  - 统计学
  - 数据分析
  - 描述性统计
category: 数据科学
keywords:
  - 描述性统计
  - 数据集中趋势
  - 数据离散程度
---

# 描述性统计

## 概述

描述性统计是数据分析的基础，它帮助我们理解数据的中心趋势、离散程度和分布形态。通过描述性统计，我们可以从数据中提取有用的信息，为后续的分析和决策提供支持。

## 1. 中心趋势

中心趋势描述了数据集的中心位置或平均水平。常用的中心趋势指标包括：

### 1.1 均值（Mean）

均值是所有数据点的总和除以数据点的数量。它是最常用的中心趋势指标。

```R
# 计算均值
data <- c(1, 2, 3, 4, 5)
mean_value <- mean(data)
print(mean_value)  # 输出: 3
```

### 1.2 中位数（Median）

中位数是将数据按大小顺序排列后位于中间的值。如果数据点的数量是偶数，则中位数是中间两个数的平均值。

```R
# 计算中位数
data <- c(1, 2, 3, 4, 5)
median_value <- median(data)
print(median_value)  # 输出: 3
```

### 1.3 众数（Mode）

众数是数据集中出现频率最高的值。R 中没有直接计算众数的函数，但可以通过 `table` 函数和 `which.max` 函数来实现。

```R
# 计算众数
data <- c(1, 2, 2, 3, 4, 5)
mode_value <- as.numeric(names(which.max(table(data))))
print(mode_value)  # 输出: 2
```

## 2. 离散程度

离散程度描述了数据点之间的差异或分散程度。常用的离散程度指标包括：

### 2.1 方差（Variance）

方差衡量数据点与均值之间的平均平方差。

```R
# 计算方差
data <- c(1, 2, 3, 4, 5)
variance_value <- var(data)
print(variance_value)  # 输出: 2.5
```

### 2.2 标准差（Standard Deviation）

标准差是方差的平方根，它衡量数据点与均值之间的平均距离。

```R
# 计算标准差
data <- c(1, 2, 3, 4, 5)
sd_value <- sd(data)
print(sd_value)  # 输出: 1.581139
```

### 2.3 范围（Range）

范围是数据集中最大值与最小值之间的差。

```R
# 计算范围
data <- c(1, 2, 3, 4, 5)
range_value <- max(data) - min(data)
print(range_value)  # 输出: 4
```

## 3. 分布形态

分布形态描述了数据的分布特征，常用的指标包括：

### 3.1 偏度（Skewness）

偏度衡量数据分布的不对称性。正偏度表示数据分布偏向右侧，负偏度表示数据分布偏向左侧。

```R
# 计算偏度
library(moments)
data <- c(1, 2, 3, 4, 5)
skewness_value <- skewness(data)
print(skewness_value)  # 输出: 0
```

### 3.2 峰度（Kurtosis）

峰度衡量数据分布的尖锐程度。正峰度表示数据分布比正态分布更尖锐，负峰度表示数据分布比正态分布更平坦。

```R
# 计算峰度
library(moments)
data <- c(1, 2, 3, 4, 5)
kurtosis_value <- kurtosis(data)
print(kurtosis_value)  # 输出: 1.7
```

## 4. 实践练习

### 练习 1: 计算一组数据的均值、中位数和众数

```R
data <- c(3, 5, 7, 7, 9, 11, 11, 11, 13)
mean_value <- mean(data)
median_value <- median(data)
mode_value <- as.numeric(names(which.max(table(data))))

print(paste("均值:", mean_value))  # 输出: 均值: 8.55555555555556
print(paste("中位数:", median_value))  # 输出: 中位数: 9
print(paste("众数:", mode_value))  # 输出: 众数: 11
```

### 练习 2: 计算一组数据的标准差和范围

```R
data <- c(2, 4, 4, 4, 5, 5, 7, 9)
sd_value <- sd(data)
range_value <- max(data) - min(data)

print(paste("标准差:", sd_value))  # 输出: 标准差: 2.13808993529939
print(paste("范围:", range_value))  # 输出: 范围: 7
```

### 练习 3: 计算一组数据的偏度和峰度

```R
library(moments)
data <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
skewness_value <- skewness(data)
kurtosis_value <- kurtosis(data)

print(paste("偏度:", skewness_value))  # 输出: 偏度: 0
print(paste("峰度:", kurtosis_value))  # 输出: 峰度: 1.77575757575758
```

## 总结

描述性统计是数据分析的第一步，它帮助我们理解数据的中心趋势、离散程度和分布形态。通过掌握这些基本概念和计算方法，我们可以更好地理解数据，为后续的分析和决策提供坚实的基础。

## 下一步

在掌握了描述性统计之后，你可以继续学习假设检验、相关分析、回归分析等更高级的统计方法。这些方法将帮助你更深入地理解数据，并从中提取更有价值的信息。