---
title: 线程的创建和生命周期详解
date: 2023-10-05
description: 本课程详细讲解如何在编程中创建和管理线程，以及线程的生命周期和关键概念。
slug: thread-creation-lifecycle
tags:
  - 多线程
  - 并发编程
  - 线程管理
category: 编程基础
keywords:
  - 线程创建
  - 线程生命周期
  - 并发编程
---

# 线程的创建和生命周期

## 1. 概述

在Java中，线程是执行程序的最小单位。通过使用线程，我们可以实现并发执行，提高程序的效率。本教程将详细介绍如何在Java中创建线程以及线程的生命周期。

## 2. 线程的创建

在Java中，创建线程有两种主要方式：

### 2.1 继承 `Thread` 类

通过继承 `Thread` 类并重写 `run()` 方法来创建线程。

```java
class MyThread extends Thread {
    @Override
    public void run() {
        System.out.println("Thread is running via extending Thread class.");
    }
}

public class Main {
    public static void main(String[] args) {
        MyThread thread = new MyThread();
        thread.start();  // 启动线程
    }
}
```

### 2.2 实现 `Runnable` 接口

通过实现 `Runnable` 接口并重写 `run()` 方法来创建线程。

```java
class MyRunnable implements Runnable {
    @Override
    public void run() {
        System.out.println("Thread is running via implementing Runnable interface.");
    }
}

public class Main {
    public static void main(String[] args) {
        MyRunnable runnable = new MyRunnable();
        Thread thread = new Thread(runnable);
        thread.start();  // 启动线程
    }
}
```

## 3. 线程的生命周期

线程的生命周期包括以下几个状态：

### 3.1 新建状态（New）

当线程对象被创建时，它处于新建状态。此时线程还没有开始执行。

```java
Thread thread = new Thread(new MyRunnable());
```

### 3.2 就绪状态（Runnable）

当调用 `start()` 方法后，线程进入就绪状态。此时线程已经准备好运行，但还未获得CPU时间片。

```java
thread.start();
```

### 3.3 运行状态（Running）

当线程获得CPU时间片后，它进入运行状态，执行 `run()` 方法中的代码。

```java
public void run() {
    System.out.println("Thread is running.");
}
```

### 3.4 阻塞状态（Blocked）

线程在某些情况下会进入阻塞状态，例如等待I/O操作完成或等待锁的释放。

```java
synchronized (lock) {
    try {
        lock.wait();  // 线程进入阻塞状态
    } catch (InterruptedException e) {
        e.printStackTrace();
    }
}
```

### 3.5 终止状态（Terminated）

当线程的 `run()` 方法执行完毕或调用 `stop()` 方法（不推荐使用）时，线程进入终止状态。

```java
public void run() {
    System.out.println("Thread is running.");
    // 线程执行完毕，进入终止状态
}
```

## 4. 实践练习

### 4.1 创建多个线程

编写一个程序，创建多个线程并让它们并发执行。

```java
public class MultiThreadExample {
    public static void main(String[] args) {
        for (int i = 0; i < 5; i++) {
            Thread thread = new Thread(new MyRunnable());
            thread.start();
        }
    }
}

class MyRunnable implements Runnable {
    @Override
    public void run() {
        System.out.println("Thread " + Thread.currentThread().getId() + " is running.");
    }
}
```

### 4.2 线程的阻塞和唤醒

编写一个程序，演示线程的阻塞和唤醒操作。

```java
public class BlockingExample {
    public static void main(String[] args) {
        Object lock = new Object();

        Thread thread1 = new Thread(() -> {
            synchronized (lock) {
                try {
                    System.out.println("Thread 1 is waiting.");
                    lock.wait();
                    System.out.println("Thread 1 is resumed.");
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        });

        Thread thread2 = new Thread(() -> {
            synchronized (lock) {
                System.out.println("Thread 2 is notifying.");
                lock.notify();
            }
        });

        thread1.start();
        thread2.start();
    }
}
```

## 5. 总结

通过本教程，我们学习了如何在Java中创建线程以及线程的生命周期。线程的创建可以通过继承 `Thread` 类或实现 `Runnable` 接口来实现。线程的生命周期包括新建、就绪、运行、阻塞和终止五个状态。掌握这些知识对于编写高效的并发程序至关重要。

## 6. 下一步

接下来，你可以学习线程的同步和锁机制，以确保多个线程之间的数据一致性。此外，线程池和并发集合也是并发编程中的重要概念，值得深入学习。

---

希望这篇教程对你理解Java中线程的创建和生命周期有所帮助！