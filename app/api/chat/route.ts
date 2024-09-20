import { NextResponse } from 'next/server';
import axios from 'axios';

const API_KEY = process.env.DEEPSEEK_API_KEY;

export async function POST(request: Request) {
  // 添加 CORS 头
  const origin = request.headers.get('origin');
  const allowedOrigins = ['https://www.coderwithai.top', 'https://coderwithai.top', 'http://localhost:3000'];
  
  if (origin && allowedOrigins.includes(origin)) {
    const headers = {
      'Access-Control-Allow-Origin': origin,
      'Access-Control-Allow-Methods': 'POST, GET, OPTIONS',
      'Access-Control-Allow-Headers': 'Content-Type',
    };

    // 处理预检请求
    if (request.method === 'OPTIONS') {
      return new Response(null, { headers });
    }

    console.log('Received request to /api/chat');
    try {
      const { messages, currentPath } = await request.json();

      console.log('Received messages:', messages);
      console.log('Current path:', currentPath);

      if (!API_KEY) {
        console.error('API_KEY is not set');
        return NextResponse.json({ error: 'API_KEY is not set' }, { status: 500 });
      }

      // 设置基础系统提示
      let systemPrompt = `你是CoderWithAI网站上一位充满激情、幽默感十足AI编程导师。用户正在 ${currentPath} 页面与你聊天，所以他们的问题可能与这个页面有关。你的使命是让编程学习变得有趣又轻松！

  记住，你是一个风趣幽默的AI，要用通俗易懂的语言解释复杂概念，就像你在跟一个没有编程背景的朋友聊天一样。适时使用生动有趣的比喻，让枯燥的代码变得生动有趣。

  引导用户思考，不要总是直接给出答案。

  记住，你的目标是激发用户的好奇心和学习热情。让他们感觉编程就像是一场充满惊喜的冒险，而你是他们可靠又有趣的向导。

  保持诚实，如果遇到不确定的问题，可以坦诚地表达。

  最后，别忘了保持幽默和热情。`;

      // 根据不同的页面路径添加特定的上下文信息
      if (currentPath.includes('/tutorials')) {
        // 教程页面的特定提示
        systemPrompt += `
        哈，看来我们的冒险者来到了教程区！准备好开启一段奇妙的编程之旅了吗？
        - 记住，这里没有笨问题，只有还没想明白的聪明问题。
        - 如果我说了什么听起来像外星语言的术语，别客气，尽管问我"这是啥？"
        - 我们会把大象（复杂问题）切成小块（简单步骤），一口一口来，保证吃得开心又不噎���。
        - 当我你看代码的时候，想象我们在一起读一本有趣的漫画书，每一行都有它的故事。`;
      } else if (currentPath === '/about') {
        // 关于页面的特定提示
        systemPrompt += `
        欢迎来到我们的秘密基地！想知道我们是一群什么样的疯狂科学家吗？
        - 我们的使命是让编程学习变得像吃棉花糖一样轻松有趣。
        - 我们相信，每个人都有潜力成为下一个编程界的超级英雄。
        - 如果你觉得学习编程和学习魔法一样神奇，那你就对了！`;
      } else if (currentPath === '/contact') {
        // 联系页面的特定提示
        systemPrompt += `
        哇，你找到了与我们联系的秘密通道！
        - 需要召唤编程精灵来帮忙吗？我来教你几个高效的咒语（提问技巧）。
        - 记住，在我们的魔法学院（教程页面）里有无数宝藏等你去发现。
        - 别忘了，每个伟大的程序员都曾是一个充满好奇的初学者，就像你现在这样！`;
      }

      // 添加通用指导原则
      systemPrompt += `
      记住，你是一个充满激情的编程导师，要遵循这些魔法原则：
      1. 保持耐心和幽默，就像你在教一只可爱但调皮的小狗狗编程。
      3. 对正确的实现给予热情洋溢的赞美，对错误给予温和幽默的纠正。
      5. 引导用户找到解决方案，就像教他们钓鱼，而不是直接给他们鱼。
      6. 尽量简洁地回答，但不要忘记加入一些俏皮话。

      记住，你的任务是让用户爱上编程，就像爱上他们最喜欢的游戏一样。让每一次交流都充满乐趣和启发，但要专注于编程话题哦！不要提到你自己是AI或者提到CoderWithAI，让我们的对话自然流畅，就像两个好朋友在聊天一样。`;

      try {
        const response = await axios.post('https://api.deepseek.com/v1/chat/completions', {
          model: "deepseek-chat",
          messages: [
            {"role": "system", "content": systemPrompt},
            ...messages.map((msg: { role: string; content: string }) => ({
              role: msg.role,
              content: msg.role === 'user' ? `"${msg.content}"` : msg.content
            }))
          ],
          stream: true
        }, {
          headers: {
            'Authorization': `Bearer ${API_KEY}`,
            'Content-Type': 'application/json'
          },
          responseType: 'stream'
        });

        console.log('Received response from DeepSeek API');

        const stream = response.data;

        return new Response(stream, {
          headers: {
            ...headers,
            'Content-Type': 'text/event-stream',
            'Cache-Control': 'no-cache',
            'Connection': 'keep-alive',
          },
        });
      } catch (error) {
        console.error('Detailed error:', error);
        if (error instanceof Error) {
          console.error('Error message:', error.message);
          console.error('Error stack:', error.stack);
        }
        return NextResponse.json({ error: 'An error occurred while processing your request.' }, { status: 500 });
      }
    } catch (error) {
      console.error('Error:', error);
      return NextResponse.json({ error: 'An error occurred while processing your request.' }, { status: 500 });
    }
  }

  // 如果不是允许的源，返回错误
  return NextResponse.json({ error: 'Not allowed' }, { status: 403 });
}