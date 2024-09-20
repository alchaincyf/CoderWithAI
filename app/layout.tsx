import type { Metadata } from "next";
import localFont from "next/font/local";
import "./globals.css";
import 'highlight.js/styles/github-dark.css';
import TutorialLayout from '@/components/tutorial-layout';
import { getAvailableLanguages } from "@/lib/tutorials";
import AIChatWidget from '@/components/AIChatWidget';
import Image from 'next/image';
import Link from 'next/link';
// 删除未使用的 LanguageSelector 导入

const geistSans = localFont({
  src: "./fonts/GeistVF.woff",
  variable: "--font-geist-sans",
  weight: "100 900",
});
const geistMono = localFont({
  src: "./fonts/GeistMonoVF.woff",
  variable: "--font-geist-mono",
  weight: "100 900",
});

export const metadata: Metadata = {
  title: "CoderWithAI",
  description: "Learn programming with our comprehensive tutorials",
};

export default async function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  const sortedLanguages = await getAvailableLanguages()

  return (
    <html lang="en" className={`${geistSans.variable} ${geistMono.variable}`}>
      <body suppressHydrationWarning={true} className="flex flex-col min-h-screen">
        <header className="bg-white shadow-md fixed top-0 left-0 right-0 z-10">
          <div className="container mx-auto px-4">
            <div className="flex items-center justify-between h-16">
              <Link href="/" className="flex items-center">
                <Image
                  src="/images/logo.png"
                  alt="CoderWithAI Logo"
                  width={40}
                  height={40}
                />
                <span className="ml-2 text-xl font-bold">CoderWithAI</span>
              </Link>
            </div>
            <nav className="py-2 border-t overflow-x-auto">
              <div className="flex space-x-4 md:justify-center">
                {sortedLanguages.map((lang) => (
                  <Link 
                    key={lang} 
                    href={`/${lang}`} 
                    className="text-gray-600 hover:text-gray-900 px-3 py-2 rounded-md text-sm font-medium whitespace-nowrap"
                  >
                    {lang}
                  </Link>
                ))}
              </div>
            </nav>
          </div>
        </header>
        <div className="flex-grow mt-32">
          <TutorialLayout>
            {children}
          </TutorialLayout>
        </div>
        <footer className="bg-gray-100 py-4 text-center text-sm text-gray-600">
          <p>Created by <a href="https://www.youtube.com/@Alchain" target="_blank" rel="noopener noreferrer" className="text-blue-600 hover:underline">AI进化论-花生</a></p>
          <p className="mt-2">友情链接：最佳AI学习网站 <a href="https://www.bookai.top/" target="_blank" rel="noopener noreferrer" className="text-blue-600 hover:underline">BookAI.TOP</a></p>
        </footer>
        <AIChatWidget />
      </body>
    </html>
  );
}
