---
title: Python 设计模式教程
date: 2023-10-05
description: 本课程深入探讨Python中的设计模式，帮助开发者理解和应用常见的设计模式，提升代码的可维护性和扩展性。
slug: python-design-patterns
tags:
  - Python
  - 设计模式
  - 编程教程
category: 编程与开发
keywords:
  - Python设计模式
  - 设计模式教程
  - Python编程
---

# Python 设计模式教程

## 1. 概述

设计模式是解决软件设计中常见问题的通用可重用解决方案。Python 作为一种灵活且强大的编程语言，支持多种设计模式。本教程将介绍几种常见的设计模式，并通过代码示例和实践练习帮助你理解和应用这些模式。

## 2. 设计模式简介

设计模式分为三大类：
- **创建型模式**：处理对象创建机制，试图以适当的方式来创建对象。
- **结构型模式**：处理对象组合或类组合，以形成更大的结构。
- **行为型模式**：处理对象之间的通信，特别是算法和职责分配。

## 3. 创建型模式

### 3.1 单例模式

**单例模式**确保一个类只有一个实例，并提供一个全局访问点。

**代码示例：**

```python
class Singleton:
    _instance = None

    def __new__(cls, *args, **kwargs):
        if not cls._instance:
            cls._instance = super(Singleton, cls).__new__(cls, *args, **kwargs)
        return cls._instance

# 测试单例模式
s1 = Singleton()
s2 = Singleton()

print(s1 is s2)  # 输出: True
```

**实践练习：**
尝试创建一个单例类 `Logger`，用于记录日志信息。

### 3.2 工厂模式

**工厂模式**提供了一种创建对象的方式，而不需要指定具体的类。

**代码示例：**

```python
class Dog:
    def speak(self):
        return "Woof!"

class Cat:
    def speak(self):
        return "Meow!"

class AnimalFactory:
    def get_animal(self, animal_type):
        if animal_type == "Dog":
            return Dog()
        elif animal_type == "Cat":
            return Cat()
        else:
            return None

# 测试工厂模式
factory = AnimalFactory()
dog = factory.get_animal("Dog")
print(dog.speak())  # 输出: Woof!
```

**实践练习：**
创建一个 `ShapeFactory`，根据输入创建不同的形状对象（如 `Circle`, `Square`）。

## 4. 结构型模式

### 4.1 适配器模式

**适配器模式**允许接口不兼容的类一起工作。

**代码示例：**

```python
class OldSystem:
    def request(self):
        return "Old System Request"

class NewSystem:
    def specific_request(self):
        return "New System Request"

class Adapter(NewSystem):
    def request(self):
        return self.specific_request()

# 测试适配器模式
old_system = OldSystem()
new_system = Adapter()

print(old_system.request())  # 输出: Old System Request
print(new_system.request())  # 输出: New System Request
```

**实践练习：**
创建一个适配器，将一个旧的支付系统适配到新的支付接口。

### 4.2 装饰器模式

**装饰器模式**动态地给对象添加职责，而不改变其结构。

**代码示例：**

```python
class Coffee:
    def cost(self):
        return 5

class MilkDecorator(Coffee):
    def __init__(self, coffee):
        self._coffee = coffee

    def cost(self):
        return self._coffee.cost() + 2

# 测试装饰器模式
coffee = Coffee()
milk_coffee = MilkDecorator(coffee)

print(coffee.cost())       # 输出: 5
print(milk_coffee.cost())  # 输出: 7
```

**实践练习：**
创建一个装饰器，用于给文本添加不同的样式（如加粗、斜体）。

## 5. 行为型模式

### 5.1 观察者模式

**观察者模式**定义了对象之间的一对多依赖关系，当一个对象改变状态时，所有依赖它的对象都会收到通知并自动更新。

**代码示例：**

```python
class Subject:
    def __init__(self):
        self._observers = []

    def attach(self, observer):
        self._observers.append(observer)

    def detach(self, observer):
        self._observers.remove(observer)

    def notify(self):
        for observer in self._observers:
            observer.update(self)

class Observer:
    def update(self, subject):
        pass

class ConcreteObserver(Observer):
    def update(self, subject):
        print("Observer notified!")

# 测试观察者模式
subject = Subject()
observer = ConcreteObserver()

subject.attach(observer)
subject.notify()  # 输出: Observer notified!
```

**实践练习：**
创建一个观察者模式，用于监控股票价格的变化。

### 5.2 策略模式

**策略模式**定义了一系列算法，并将每个算法封装起来，使它们可以互换。

**代码示例：**

```python
class Strategy:
    def execute(self):
        pass

class ConcreteStrategyA(Strategy):
    def execute(self):
        print("Executing Strategy A")

class ConcreteStrategyB(Strategy):
    def execute(self):
        print("Executing Strategy B")

class Context:
    def __init__(self, strategy):
        self._strategy = strategy

    def execute_strategy(self):
        self._strategy.execute()

# 测试策略模式
context = Context(ConcreteStrategyA())
context.execute_strategy()  # 输出: Executing Strategy A

context._strategy = ConcreteStrategyB()
context.execute_strategy()  # 输出: Executing Strategy B
```

**实践练习：**
创建一个策略模式，用于选择不同的排序算法（如冒泡排序、快速排序）。

## 6. 总结

本教程介绍了 Python 中几种常见的设计模式，并通过代码示例和实践练习帮助你理解和应用这些模式。设计模式是软件设计中的重要工具，掌握它们将有助于你编写更灵活、可维护的代码。

## 7. 进一步学习

- 深入学习每种设计模式的变体和应用场景。
- 探索其他设计模式，如代理模式、模板方法模式等。
- 阅读《设计模式：可复用面向对象软件的基础》（Gang of Four）一书，了解更多设计模式的知识。

希望本教程对你有所帮助，祝你在 Python 编程和设计模式的学习中取得进步！