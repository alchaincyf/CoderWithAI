import { NextResponse } from 'next/server';
import axios from 'axios';

const API_KEY = process.env.DEEPSEEK_API_KEY;

export async function POST(request: Request) {
  try {
    const { messages } = await request.json();

    console.log('Received messages:', messages);

    if (!API_KEY) {
      console.error('API_KEY is not set');
      throw new Error('API_KEY is not set');
    }

    const response = await axios.post('https://api.deepseek.com/v1/chat/completions', {
      model: "deepseek-chat",
      messages: [
        {"role": "system", "content": "你是一个AI助手"},
        ...messages.map((msg: { role: string; content: string }) => ({
          role: msg.role,
          content: msg.role === 'user' ? `"${msg.content}"` : msg.content
        }))
      ],
      stream: false
    }, {
      headers: {
        'Authorization': `Bearer ${API_KEY}`,
        'Content-Type': 'application/json'
      }
    });

    console.log('API response:', response.data);

    return NextResponse.json({ message: response.data.choices[0].message.content });
  } catch (error) {
    console.error('Detailed error:', error);
    if (axios.isAxiosError(error)) {
      console.error('Axios error:', error.response?.data);
    }
    return NextResponse.json({ error: 'An error occurred while processing your request.' }, { status: 500 });
  }
}