---
title: 多线程编程入门：掌握并发与并行
date: 2023-10-05
description: 本课程将深入探讨多线程编程的基础知识，帮助你理解并发与并行的概念，并掌握如何在不同编程语言中实现多线程支持。
slug: multithreading-support-thread
tags:
  - 多线程
  - 并发编程
  - 并行计算
category: 编程技术
keywords:
  - 多线程编程
  - 并发
  - 并行
---

# 多线程支持 (thread)

## 1. 概述

在现代编程中，多线程是一个非常重要的概念。多线程允许程序同时执行多个任务，从而提高程序的效率和响应速度。C++11 引入了标准库中的 `std::thread` 类，使得多线程编程变得更加简单和安全。

## 2. 基本概念

### 2.1 线程 (Thread)

线程是操作系统能够进行运算调度的最小单位。它被包含在进程之中，是进程中的实际运作单位。一个进程可以包含多个线程，这些线程共享进程的资源，如内存空间。

### 2.2 并发 (Concurrency)

并发是指多个任务在同一时间段内交替执行，给人一种同时执行的错觉。并发可以通过多线程或多进程实现。

### 2.3 并行 (Parallelism)

并行是指多个任务真正同时执行，通常需要多个处理器或核心。

## 3. 创建线程

在 C++ 中，创建线程非常简单。我们可以使用 `std::thread` 类来创建和管理线程。

### 3.1 基本用法

```cpp
#include <iostream>
#include <thread>

void threadFunction() {
    std::cout << "Hello from thread!" << std::endl;
}

int main() {
    std::thread t(threadFunction);
    t.join();  // 等待线程完成
    std::cout << "Thread finished" << std::endl;
    return 0;
}
```

### 3.2 传递参数

你可以向线程函数传递参数：

```cpp
#include <iostream>
#include <thread>

void threadFunction(int x) {
    std::cout << "Value: " << x << std::endl;
}

int main() {
    std::thread t(threadFunction, 42);
    t.join();
    std::cout << "Thread finished" << std::endl;
    return 0;
}
```

## 4. 线程管理

### 4.1 `join()` 和 `detach()`

- `join()`：主线程会等待子线程执行完毕后再继续执行。
- `detach()`：主线程不会等待子线程，子线程会在后台运行。

```cpp
#include <iostream>
#include <thread>

void threadFunction() {
    std::cout << "Hello from thread!" << std::endl;
}

int main() {
    std::thread t(threadFunction);
    t.detach();  // 分离线程
    std::cout << "Thread detached" << std::endl;
    return 0;
}
```

### 4.2 线程标识

每个线程都有一个唯一的标识符，可以通过 `std::thread::id` 获取。

```cpp
#include <iostream>
#include <thread>

void threadFunction() {
    std::cout << "Thread ID: " << std::this_thread::get_id() << std::endl;
}

int main() {
    std::thread t(threadFunction);
    t.join();
    std::cout << "Main Thread ID: " << std::this_thread::get_id() << std::endl;
    return 0;
}
```

## 5. 线程同步

多线程编程中，线程同步是一个重要的问题。线程同步可以防止数据竞争和确保线程安全。

### 5.1 互斥锁 (Mutex)

互斥锁用于保护共享资源，确保同一时间只有一个线程可以访问该资源。

```cpp
#include <iostream>
#include <thread>
#include <mutex>

std::mutex mtx;

void threadFunction(int id) {
    mtx.lock();
    std::cout << "Thread " << id << " is working" << std::endl;
    mtx.unlock();
}

int main() {
    std::thread t1(threadFunction, 1);
    std::thread t2(threadFunction, 2);
    t1.join();
    t2.join();
    return 0;
}
```

### 5.2 条件变量 (Condition Variable)

条件变量用于线程间的通信，允许一个线程等待另一个线程的通知。

```cpp
#include <iostream>
#include <thread>
#include <mutex>
#include <condition_variable>

std::mutex mtx;
std::condition_variable cv;
bool ready = false;

void workerThread() {
    std::unique_lock<std::mutex> lock(mtx);
    cv.wait(lock, []{ return ready; });
    std::cout << "Worker thread is working" << std::endl;
}

void mainThread() {
    std::this_thread::sleep_for(std::chrono::seconds(1));
    {
        std::lock_guard<std::mutex> lock(mtx);
        ready = true;
    }
    cv.notify_one();
}

int main() {
    std::thread worker(workerThread);
    std::thread main(mainThread);
    worker.join();
    main.join();
    return 0;
}
```

## 6. 实践练习

### 6.1 练习1：计算素数

编写一个程序，使用多线程计算一定范围内的素数。主线程负责接收用户输入的范围，并创建多个子线程来计算素数。

### 6.2 练习2：生产者-消费者问题

实现一个简单的生产者-消费者模型。生产者线程生成数据并放入队列，消费者线程从队列中取出数据并处理。

## 7. 总结

多线程编程是现代编程中不可或缺的一部分。通过使用 C++ 的标准库 `std::thread`，我们可以轻松地创建和管理线程。然而，多线程编程也带来了线程同步和数据竞争等问题，需要我们谨慎处理。

通过本教程的学习，你应该已经掌握了 C++ 中多线程编程的基本概念和技巧。希望你能继续深入学习，掌握更多高级的多线程编程技术。