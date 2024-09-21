import { getAvailableLanguages, getTutorials } from "@/lib/tutorials";

export default async function sitemap() {
  const baseUrl = "https://www.codewithai.com";
  const languages = await getAvailableLanguages();

  const urls: string[] = [];

  for (const lang of languages) {
    const tutorials = await getTutorials(lang);
    urls.push(`${baseUrl}/${lang}`);
    tutorials.forEach((tutorial) => {
      urls.push(`${baseUrl}/${lang}/${tutorial.slug}`);
    });
  }

  return urls;
}