export interface Message {
  content: string;
  role: 'user' | 'assistant';
}

export interface Tutorial {
  title: string;
  path: string;
  description?: string;
  image?: string;
  items?: Tutorial[];
  // 添加其他必要的属性
}

// 添加其他需要的接口或类型
