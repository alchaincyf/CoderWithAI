import type { Metadata } from "next";
import localFont from "next/font/local";
import "./globals.css";
import 'highlight.js/styles/github-dark.css';
import TutorialLayout from '@/components/tutorial-layout';
import { getAvailableLanguages } from "@/lib/tutorials";

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
  title: "CodeWithAI",
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
      <body suppressHydrationWarning={true}>
        <TutorialLayout languages={sortedLanguages}>
          {children}
        </TutorialLayout>
      </body>
    </html>
  );
}
