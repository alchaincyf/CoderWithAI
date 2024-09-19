---
title: 同步原语：Mutex与WaitGroup详解
date: 2023-10-05
description: 本课程深入探讨了编程中的同步原语，重点介绍了Mutex和WaitGroup的使用方法及其在并发编程中的重要性。
slug: synchronization-primitives-mutex-waitgroup
tags:
  - 并发编程
  - 同步原语
  - Go语言
category: 编程基础
keywords:
  - Mutex
  - WaitGroup
  - 并发控制
---

# 同步原语 (Mutex, WaitGroup)

在并发编程中，确保多个 Goroutines 之间的数据同步和协调是非常重要的。Go 语言提供了多种同步原语来帮助开发者实现这一目标，其中最常用的就是 `Mutex` 和 `WaitGroup`。

## 1. Mutex (互斥锁)

### 1.1 理论解释

`Mutex` 是 "Mutual Exclusion" 的缩写，即互斥锁。它用于保护共享资源，确保在同一时间只有一个 Goroutine 可以访问该资源。`Mutex` 有两种状态：锁定和未锁定。当一个 Goroutine 锁定 `Mutex` 时，其他 Goroutine 必须等待该 `Mutex` 被解锁后才能继续执行。

### 1.2 代码示例

```go
package main

import (
	"fmt"
	"sync"
)

var (
	counter int
	mutex   sync.Mutex
)

func increment() {
	mutex.Lock()
	defer mutex.Unlock()
	counter++
}

func main() {
	var wg sync.WaitGroup

	for i := 0; i < 1000; i++ {
		wg.Add(1)
		go func() {
			defer wg.Done()
			increment()
		}()
	}

	wg.Wait()
	fmt.Println("Counter:", counter)
}
```

### 1.3 解释

- `mutex.Lock()`：锁定 `Mutex`，确保只有一个 Goroutine 可以访问 `counter`。
- `defer mutex.Unlock()`：在函数返回前解锁 `Mutex`，确保其他 Goroutine 可以访问 `counter`。
- `counter++`：在 `Mutex` 的保护下，安全地增加 `counter` 的值。

### 1.4 实践练习

修改上述代码，增加一个 `decrement` 函数，并使用 `Mutex` 保护 `counter` 的减少操作。

## 2. WaitGroup

### 2.1 理论解释

`WaitGroup` 用于等待一组 Goroutines 完成。它通过 `Add`、`Done` 和 `Wait` 三个方法来实现：

- `Add(delta int)`：增加等待的 Goroutines 数量。
- `Done()`：减少等待的 Goroutines 数量。
- `Wait()`：阻塞直到所有 Goroutines 完成。

### 2.2 代码示例

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

func worker(id int, wg *sync.WaitGroup) {
	defer wg.Done()
	fmt.Printf("Worker %d starting\n", id)
	time.Sleep(time.Second)
	fmt.Printf("Worker %d done\n", id)
}

func main() {
	var wg sync.WaitGroup

	for i := 1; i <= 5; i++ {
		wg.Add(1)
		go worker(i, &wg)
	}

	wg.Wait()
	fmt.Println("All workers done")
}
```

### 2.3 解释

- `wg.Add(1)`：增加一个等待的 Goroutine。
- `defer wg.Done()`：在函数返回前减少一个等待的 Goroutine。
- `wg.Wait()`：阻塞直到所有 Goroutines 完成。

### 2.4 实践练习

修改上述代码，增加一个 `worker` 函数的参数，用于指定每个 `worker` 的工作时间（例如，`worker(id, timeToWork, wg)`），并使用 `WaitGroup` 等待所有 `worker` 完成。

## 3. 综合应用

### 3.1 理论解释

在实际项目中，`Mutex` 和 `WaitGroup` 通常会结合使用，以确保多个 Goroutines 之间的数据同步和任务协调。

### 3.2 代码示例

```go
package main

import (
	"fmt"
	"sync"
	"time"
)

var (
	counter int
	mutex   sync.Mutex
)

func worker(id int, wg *sync.WaitGroup) {
	defer wg.Done()
	fmt.Printf("Worker %d starting\n", id)
	time.Sleep(time.Second)

	mutex.Lock()
	counter++
	mutex.Unlock()

	fmt.Printf("Worker %d done\n", id)
}

func main() {
	var wg sync.WaitGroup

	for i := 1; i <= 5; i++ {
		wg.Add(1)
		go worker(i, &wg)
	}

	wg.Wait()
	fmt.Println("All workers done")
	fmt.Println("Counter:", counter)
}
```

### 3.3 解释

- `mutex.Lock()` 和 `mutex.Unlock()`：保护 `counter` 的增加操作。
- `wg.Add(1)`、`wg.Done()` 和 `wg.Wait()`：确保所有 `worker` 完成后再输出 `counter` 的值。

### 3.4 实践练习

修改上述代码，增加一个 `counter` 的减少操作，并确保在所有 `worker` 完成后再输出 `counter` 的值。

## 4. 总结

`Mutex` 和 `WaitGroup` 是 Go 语言中非常重要的同步原语，它们帮助开发者实现并发程序中的数据同步和任务协调。通过合理使用这些工具，可以避免常见的并发问题，如数据竞争和死锁。

希望这篇教程能帮助你更好地理解和应用 `Mutex` 和 `WaitGroup`，并在实际项目中发挥它们的作用。