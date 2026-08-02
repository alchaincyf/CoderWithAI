import { LanguageProvider } from './LanguageProvider'
import LanguageLayoutClient from './LanguageLayoutClient'
import { notFound } from 'next/navigation'

export default async function LanguageLayoutServer({
  children,
  params
}: {
  children: React.ReactNode,
  params: { language: string }
}) {
  const data = await LanguageProvider({ language: params.language })
  // 不存在的语言目录直接 404，避免下游组件拿到空数据或 readdirSync 抛 ENOENT 变成 500
  if (data.tutorials.length === 0) {
    notFound()
  }

  return (
    <LanguageLayoutClient tutorials={data.tutorials} language={data.language}>
      {children}
    </LanguageLayoutClient>
  )
}