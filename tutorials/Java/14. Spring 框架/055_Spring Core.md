---
title: Spring Core 基础教程
date: 2023-10-05
description: 本课程深入讲解Spring Core框架的核心概念和基本用法，帮助开发者掌握Spring的核心组件和依赖注入机制。
slug: spring-core-tutorial
tags:
  - Spring
  - Java
  - 依赖注入
category: 编程教程
keywords:
  - Spring Core
  - 依赖注入
  - Java框架
---

# Spring Core 教程

## 1. 概述

Spring Core 是 Spring 框架的核心模块，提供了依赖注入（Dependency Injection, DI）和控制反转（Inversion of Control, IoC）的核心功能。通过 Spring Core，开发者可以更容易地管理对象的生命周期和依赖关系，从而提高代码的可维护性和可测试性。

## 2. 控制反转（IoC）

### 2.1 什么是控制反转？

控制反转是一种设计原则，它将对象的创建和依赖关系的管理从应用程序代码中移除，并将其交给一个外部容器（如 Spring 容器）来处理。这样，应用程序代码只需要关注业务逻辑，而不需要关心对象的创建和管理。

### 2.2 为什么使用控制反转？

- **解耦**：减少对象之间的直接依赖关系，使代码更灵活。
- **可测试性**：更容易编写单元测试，因为依赖关系可以被模拟或替换。
- **可维护性**：代码更易于维护和扩展。

## 3. 依赖注入（DI）

### 3.1 什么是依赖注入？

依赖注入是实现控制反转的一种方式。它通过外部容器将依赖对象注入到需要它们的对象中，而不是让对象自己创建或查找依赖。

### 3.2 依赖注入的方式

- **构造器注入**：通过构造器传递依赖对象。
- **Setter 注入**：通过 setter 方法传递依赖对象。
- **字段注入**：通过直接在字段上使用注解注入依赖对象。

### 3.3 代码示例

#### 3.3.1 构造器注入

```java
public class UserService {
    private final UserRepository userRepository;

    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public void saveUser(User user) {
        userRepository.save(user);
    }
}
```

#### 3.3.2 Setter 注入

```java
public class UserService {
    private UserRepository userRepository;

    public void setUserRepository(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public void saveUser(User user) {
        userRepository.save(user);
    }
}
```

#### 3.3.3 字段注入

```java
public class UserService {
    @Autowired
    private UserRepository userRepository;

    public void saveUser(User user) {
        userRepository.save(user);
    }
}
```

## 4. Spring 容器

### 4.1 什么是 Spring 容器？

Spring 容器是 Spring 框架的核心组件，负责管理对象的创建、配置和生命周期。Spring 容器通过读取配置文件或注解来创建和管理对象，并将依赖注入到需要的对象中。

### 4.2 Spring 容器的类型

- **BeanFactory**：最基本的 Spring 容器，提供了基本的 IoC 功能。
- **ApplicationContext**：BeanFactory 的扩展，提供了更多的企业级功能，如 AOP、国际化、事件发布等。

### 4.3 配置 Spring 容器

Spring 容器可以通过 XML 配置文件、Java 配置类或注解来配置。

#### 4.3.1 XML 配置

```xml
<beans xmlns="http://www.springframework.org/schema/beans"
       xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
       xsi:schemaLocation="http://www.springframework.org/schema/beans
                           http://www.springframework.org/schema/beans/spring-beans.xsd">

    <bean id="userRepository" class="com.example.UserRepositoryImpl"/>

    <bean id="userService" class="com.example.UserService">
        <constructor-arg ref="userRepository"/>
    </bean>
</beans>
```

#### 4.3.2 Java 配置

```java
@Configuration
public class AppConfig {

    @Bean
    public UserRepository userRepository() {
        return new UserRepositoryImpl();
    }

    @Bean
    public UserService userService(UserRepository userRepository) {
        return new UserService(userRepository);
    }
}
```

#### 4.3.3 注解配置

```java
@Component
public class UserRepositoryImpl implements UserRepository {
    // ...
}

@Service
public class UserService {
    private final UserRepository userRepository;

    @Autowired
    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public void saveUser(User user) {
        userRepository.save(user);
    }
}
```

## 5. 实践练习

### 5.1 创建一个简单的 Spring 应用程序

1. **创建 Maven 项目**：使用 Maven 创建一个新的 Java 项目。
2. **添加 Spring 依赖**：在 `pom.xml` 中添加 Spring Core 依赖。
3. **创建配置类**：创建一个 Java 配置类，定义 Bean。
4. **创建服务类**：创建一个服务类，并使用依赖注入。
5. **运行应用程序**：使用 Spring 容器加载配置并运行应用程序。

### 5.2 代码示例

```java
// AppConfig.java
@Configuration
public class AppConfig {

    @Bean
    public UserRepository userRepository() {
        return new UserRepositoryImpl();
    }

    @Bean
    public UserService userService(UserRepository userRepository) {
        return new UserService(userRepository);
    }
}

// UserRepository.java
public interface UserRepository {
    void save(User user);
}

// UserRepositoryImpl.java
public class UserRepositoryImpl implements UserRepository {
    @Override
    public void save(User user) {
        System.out.println("Saving user: " + user);
    }
}

// UserService.java
public class UserService {
    private final UserRepository userRepository;

    public UserService(UserRepository userRepository) {
        this.userRepository = userRepository;
    }

    public void saveUser(User user) {
        userRepository.save(user);
    }
}

// Main.java
public class Main {
    public static void main(String[] args) {
        ApplicationContext context = new AnnotationConfigApplicationContext(AppConfig.class);
        UserService userService = context.getBean(UserService.class);
        userService.saveUser(new User("John Doe"));
    }
}
```

## 6. 总结

通过本教程，我们学习了 Spring Core 的核心概念，包括控制反转、依赖注入以及 Spring 容器的配置和使用。Spring Core 是 Spring 框架的基础，掌握它对于理解和使用 Spring 框架的其他模块至关重要。

希望本教程能帮助你更好地理解 Spring Core，并在实际项目中应用这些知识。继续学习和实践，你将能够构建更强大、更灵活的应用程序。