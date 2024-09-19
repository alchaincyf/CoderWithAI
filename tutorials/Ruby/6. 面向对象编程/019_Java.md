---
title: 方法可见性详解：Java中的访问控制
date: 2023-10-05
description: 本课程详细讲解Java编程语言中的方法可见性，包括public、private、protected和默认访问修饰符的使用与区别。
slug: java-method-visibility
tags:
  - Java
  - 面向对象编程
  - 访问控制
category: 编程基础
keywords:
  - Java方法可见性
  - 访问修饰符
  - 面向对象编程
---

# 方法可见性

在 Ruby 中，方法的可见性（visibility）决定了哪些代码可以调用这些方法。理解方法可见性对于编写模块化、可维护的代码至关重要。Ruby 提供了三种方法可见性：`public`、`private` 和 `protected`。

## 1. 公共方法 (`public`)

公共方法是默认的可见性，可以在任何地方调用。它们通常用于类的外部接口，供其他对象或类使用。

### 代码示例

```ruby
class MyClass
  def public_method
    puts "This is a public method"
  end
end

obj = MyClass.new
obj.public_method  # 输出: This is a public method
```

### 解释

- `public_method` 是一个公共方法，可以在类的外部调用。
- 任何对象都可以调用这个方法。

## 2. 私有方法 (`private`)

私有方法只能在类的内部调用，不能在类的外部或通过对象实例调用。私有方法通常用于类的内部实现细节，不希望被外部访问。

### 代码示例

```ruby
class MyClass
  def public_method
    puts "This is a public method"
    private_method  # 在内部调用私有方法
  end

  private

  def private_method
    puts "This is a private method"
  end
end

obj = MyClass.new
obj.public_method  # 输出: This is a public method
                   # 输出: This is a private method

# obj.private_method  # 这行代码会导致错误，因为私有方法不能在类的外部调用
```

### 解释

- `private_method` 是一个私有方法，只能在类的内部调用。
- 在 `public_method` 内部调用 `private_method` 是合法的。
- 尝试在类的外部调用 `private_method` 会导致错误。

## 3. 受保护方法 (`protected`)

受保护方法可以在类的内部以及子类中调用，但不能在类的外部调用。受保护方法通常用于类的内部实现细节，但允许子类访问。

### 代码示例

```ruby
class MyClass
  def public_method
    puts "This is a public method"
    protected_method  # 在内部调用受保护方法
  end

  protected

  def protected_method
    puts "This is a protected method"
  end
end

class SubClass < MyClass
  def call_protected
    protected_method  # 在子类中调用受保护方法
  end
end

obj = MyClass.new
obj.public_method  # 输出: This is a public method
                   # 输出: This is a protected method

sub_obj = SubClass.new
sub_obj.call_protected  # 输出: This is a protected method

# obj.protected_method  # 这行代码会导致错误，因为受保护方法不能在类的外部调用
```

### 解释

- `protected_method` 是一个受保护方法，可以在类的内部和子类中调用。
- 在 `public_method` 内部调用 `protected_method` 是合法的。
- 在子类 `SubClass` 中调用 `protected_method` 也是合法的。
- 尝试在类的外部调用 `protected_method` 会导致错误。

## 4. 实践练习

### 练习 1: 定义一个类并设置方法可见性

1. 创建一个名为 `Person` 的类。
2. 定义一个公共方法 `greet`，它输出 "Hello!"。
3. 定义一个私有方法 `secret`，它输出 "This is a secret!"。
4. 在 `greet` 方法中调用 `secret` 方法。
5. 创建一个 `Person` 类的实例并调用 `greet` 方法。

### 练习 2: 使用受保护方法

1. 创建一个名为 `Employee` 的类，继承自 `Person` 类。
2. 在 `Employee` 类中定义一个受保护方法 `salary`，它输出 "Salary is confidential!"。
3. 在 `Employee` 类中定义一个公共方法 `display_salary`，它调用 `salary` 方法。
4. 创建一个 `Employee` 类的实例并调用 `display_salary` 方法。

### 练习 3: 尝试调用私有和受保护方法

1. 尝试在 `Person` 类的外部调用 `secret` 方法。
2. 尝试在 `Employee` 类的外部调用 `salary` 方法。
3. 观察并记录错误信息。

## 5. 总结

方法可见性是 Ruby 中控制方法访问权限的重要机制。通过合理使用 `public`、`private` 和 `protected`，可以提高代码的封装性和可维护性。公共方法用于类的外部接口，私有方法用于内部实现细节，受保护方法用于允许子类访问的内部实现细节。

通过实践练习，你将更好地理解如何在实际项目中应用这些概念。