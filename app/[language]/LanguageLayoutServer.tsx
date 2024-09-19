import { LanguageProvider } from './LanguageProvider'
import LanguageLayoutClient from './LanguageLayoutClient'

export default async function LanguageLayoutServer({ 
  children,
  params 
}: { 
  children: React.ReactNode,
  params: { language: string }
}) {
  const data = await LanguageProvider({ language: params.language })

  return (
    <LanguageLayoutClient tutorials={data.tutorials} language={data.language}>
      {children}
    </LanguageLayoutClient>
  )
}