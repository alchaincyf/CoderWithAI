// 导入必要的模块
import { getTutorialContent } from "@/lib/tutorials"
import ReactMarkdown from 'react-markdown'
import path from 'path'

// 定义TutorialPage组件，这是一个异步的React组件
export default async function TutorialPage({ params }: { params: { language: string, tutorial: string[] } }) {
  // 从路由参数中提取语言和教程路径
  const language = params.language
  const tutorialPath = params.tutorial.join('/')

  // 输出调试信息
  console.log('TutorialPage component:')
  console.log('params:', params)
  console.log('language:', language)
  console.log('tutorialPath:', tutorialPath)
  
  // 构建并输出预期的文件路径（仅用于调试）
  console.log(`Fetching content for language: ${language}, tutorial: ${tutorialPath}`)
  console.log(`Expected file path: ${path.join(process.cwd(), 'tutorials', language, `${tutorialPath}.md`)}`)
  
  // 调用getTutorialContent函数获取教程内容
  const content = await getTutorialContent(language, tutorialPath)
  console.log(`Content fetched, length: ${content.length}`)

  // 检查是否成功获取内容
  if (content === 'Tutorial content not found.' || content === 'Error loading tutorial content.') {
    console.error('Content not found or error loading content')
    return <div className="text-red-500">{content}</div>
  }

  // 如果成功获取内容，渲染教程内容
  return (
    <div className="prose max-w-none">
      <ReactMarkdown>{content}</ReactMarkdown>
    </div>
  )
}