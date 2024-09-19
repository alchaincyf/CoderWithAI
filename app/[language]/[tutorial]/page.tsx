// 导入所需的模块
import { getTutorialContent } from "@/lib/tutorials"  // 导入获取教程内容的函数
import ReactMarkdown from 'react-markdown'  // 导入用于渲染Markdown的组件
import path from 'path'  // 导入用于处理文件路径的模块

// 定义TutorialPage组件，这是一个异步函数组件
export default async function TutorialPage({ params }: { params: { language: string, tutorial: string[] } }) {
  // 从params中提取language和tutorial参数
  const language = params.language
  // 如果tutorial是数组，则将其join成字符串；否则直接使用
  const tutorialPath = Array.isArray(params.tutorial) ? params.tutorial.join('/') : params.tutorial

  // 打印日志，显示正在获取的内容信息
  console.log(`Fetching content for language: ${language}, tutorial: ${tutorialPath}`)
  // 打印完整的文件路径
  console.log(`Full path: ${path.join(process.cwd(), 'tutorials', language, `${tutorialPath}.md`)}`)
  
  // 调用getTutorialContent函数获取教程内容
  const content = await getTutorialContent(language, tutorialPath)
  // 打印获取到的内容长度
  console.log(`Content fetched, length: ${content.length}`)

  // 如果内容获取失败，返回错误信息
  if (content === 'Tutorial content not found.' || content === 'Error loading tutorial content.') {
    return <div className="text-red-500">{content}</div>
  }

  // 如果内容获取成功，渲染教程内容
  return (
    <div className="prose max-w-none">
      <ReactMarkdown>{content}</ReactMarkdown>
    </div>
  )
}