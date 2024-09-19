---
title: 深入理解Swift中的关联值与原始值
date: 2023-10-05
description: 本课程详细讲解Swift中枚举类型的关联值和原始值的概念、用途及实现方法，帮助开发者更好地理解和使用Swift语言。
slug: swift-associated-values-raw-values
tags:
  - Swift
  - 枚举
  - 编程基础
category: 编程语言
keywords:
  - Swift关联值
  - Swift原始值
  - Swift枚举
---

# 关联值和原始值

在 Swift 中，枚举（Enum）是一种非常强大的数据类型，它允许我们定义一组相关的值。枚举不仅可以存储简单的原始值（Raw Values），还可以存储复杂的关联值（Associated Values）。本教程将详细介绍这两种概念，并通过代码示例和实践练习帮助你更好地理解它们。

## 1. 枚举简介

枚举是一种自定义数据类型，用于定义一组相关的值。例如，我们可以定义一个表示一周中各天的枚举：

```swift
enum Weekday {
    case monday
    case tuesday
    case wednesday
    case thursday
    case friday
    case saturday
    case sunday
}
```

在这个例子中，`Weekday` 枚举定义了七个可能的值，分别表示一周中的每一天。

## 2. 原始值（Raw Values）

原始值是枚举中每个成员的默认值。这些值可以是字符串、字符、整数或浮点数。原始值在枚举定义时就已经确定，并且每个成员的原始值必须是唯一的。

### 2.1 定义带有原始值的枚举

我们可以为 `Weekday` 枚举添加原始值，例如使用整数表示一周中的天数：

```swift
enum Weekday: Int {
    case monday = 1
    case tuesday = 2
    case wednesday = 3
    case thursday = 4
    case friday = 5
    case saturday = 6
    case sunday = 7
}
```

在这个例子中，`Weekday` 枚举的每个成员都有一个对应的整数原始值。

### 2.2 访问原始值

我们可以通过 `rawValue` 属性来访问枚举成员的原始值：

```swift
let day = Weekday.monday
print(day.rawValue)  // 输出: 1
```

### 2.3 从原始值创建枚举实例

我们还可以通过原始值来创建枚举实例：

```swift
if let day = Weekday(rawValue: 3) {
    print(day)  // 输出: wednesday
}
```

如果原始值不存在，`Weekday(rawValue: 3)` 将返回 `nil`，因此我们使用 `if let` 来安全地解包可选值。

## 3. 关联值（Associated Values）

关联值允许我们将自定义数据附加到枚举的每个成员上。与原始值不同，关联值在枚举实例创建时才确定，并且可以包含不同类型的数据。

### 3.1 定义带有关联值的枚举

假设我们有一个表示支付方式的枚举，每种支付方式可能包含不同的关联值：

```swift
enum PaymentMethod {
    case creditCard(cardNumber: String, expirationDate: String)
    case paypal(email: String)
    case cash
}
```

在这个例子中，`creditCard` 和 `paypal` 成员带有关联值，而 `cash` 成员没有关联值。

### 3.2 使用关联值

我们可以创建带有关联值的枚举实例：

```swift
let payment = PaymentMethod.creditCard(cardNumber: "1234-5678-9012-3456", expirationDate: "12/25")
```

### 3.3 提取关联值

我们可以使用 `switch` 语句来提取和处理关联值：

```swift
switch payment {
case .creditCard(let cardNumber, let expirationDate):
    print("Paying with credit card: \(cardNumber), expires on \(expirationDate)")
case .paypal(let email):
    print("Paying with PayPal: \(email)")
case .cash:
    print("Paying with cash")
}
```

在这个例子中，`switch` 语句根据支付方式提取并处理关联值。

## 4. 实践练习

### 4.1 练习：定义一个带有原始值的枚举

定义一个表示月份的枚举 `Month`，并为每个成员添加原始值（使用整数表示月份）。然后，编写代码访问某个成员的原始值，并从原始值创建枚举实例。

### 4.2 练习：定义一个带有关联值的枚举

定义一个表示订单状态的枚举 `OrderStatus`，包含以下成员：
- `pending`（待处理）
- `shipped`（已发货，带有发货日期和物流公司）
- `delivered`（已送达，带有送达日期）

编写代码创建一个 `shipped` 状态的订单实例，并使用 `switch` 语句提取和处理关联值。

## 5. 总结

通过本教程，我们学习了 Swift 中枚举的两种重要特性：原始值和关联值。原始值用于为枚举成员提供默认值，而关联值允许我们将自定义数据附加到枚举成员上。这两种特性使得枚举在处理复杂数据时更加灵活和强大。

希望本教程能帮助你更好地理解和使用 Swift 中的枚举。继续练习和探索，你将能够更熟练地应用这些概念。