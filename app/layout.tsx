import type { Metadata } from "next";
import localFont from "next/font/local";
import "./globals.css";
import 'highlight.js/styles/github-dark.css';
import TutorialLayout from '@/components/tutorial-layout';
import { getAvailableLanguages } from "@/lib/tutorials";
import AIChatWidget from '@/components/AIChatWidget';
import Image from 'next/image';
import Link from 'next/link';
import GoogleAnalytics from './GoogleAnalytics';
import Script from 'next/script';

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
  title: "CoderWithAI - 学习编程的最佳资源",
  description: "CoderWithAI 提供全面的编程教程，涵盖多种编程语言和框架。通过我们的交互式学习平台，提升您的编程技能。",
  keywords: "编程教程, 学习编码, 软件开发, Web开发, AI编程",
  openGraph: {
    title: "CoderWithAI - 学习编程的最佳资源",
    description: "全面的编程教程和交互式学习平台",
    url: "https://www.codewithai.com",
    siteName: "CoderWithAI",
    images: [
      {
        url: "https://www.codewithai.com/images/og-image.jpg",
        width: 1200,
        height: 630,
      },
    ],
    locale: "zh_CN",
    type: "website",
  },
  twitter: {
    card: "summary_large_image",
    title: "CoderWithAI - 学习编程的最佳资源",
    description: "全面的编程教程和交互式学习平台",
    images: ["https://www.codewithai.com/images/twitter-image.jpg"],
  },
};

export default async function RootLayout({
  children,
}: Readonly<{
  children: React.ReactNode;
}>) {
  const sortedLanguages = await getAvailableLanguages()

  return (
    <html lang="zh-CN" className={`${geistSans.variable} ${geistMono.variable}`}>
      <head>
        <GoogleAnalytics />
        <Script
          src={`https://www.googletagmanager.com/gtag/js?id=GT-KDD8ZDP3`}
          strategy="afterInteractive"
        />
        <Script id="google-tag-manager" strategy="afterInteractive">
          {`
            (function(w,d,s,l,i){w[l]=w[l]||[];w[l].push({'gtm.start':
            new Date().getTime(),event:'gtm.js'});var f=d.getElementsByTagName(s)[0],
            j=d.createElement(s),dl=l!='dataLayer'?'&l='+l:'';j.async=true;j.src=
            'https://www.googletagmanager.com/gtm.js?id='+i+dl;f.parentNode.insertBefore(j,f);
            })(window,document,'script','dataLayer','GT-KDD8ZDP3');
          `}
        </Script>
        <Script
          async
          src="https://pagead2.googlesyndication.com/pagead/js/adsbygoogle.js?client=ca-pub-7522094408813551"
          crossOrigin="anonymous"
        />
        <Script id="schema-org" type="application/ld+json">
          {`
            {
              "@context": "https://schema.org",
              "@type": "WebSite",
              "name": "CoderWithAI",
              "url": "https://www.codewithai.com",
              "description": "CoderWithAI 提供全面的编程教程，涵盖多种编程语言和框架。通过我们的交互式学习平台，提升您的编程技能。",
              "potentialAction": {
                "@type": "SearchAction",
                "target": "https://www.codewithai.com/search?q={search_term_string}",
                "query-input": "required name=search_term_string"
              }
            }
          `}
        </Script>
      </head>
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
                  priority
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
