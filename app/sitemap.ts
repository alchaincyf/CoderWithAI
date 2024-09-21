import { getAvailableLanguages, getTutorials } from "@/lib/tutorials";

export default async function sitemap() {
  const baseUrl = "https://www.codewithai.com";
  const languages = await getAvailableLanguages();

  let urls = [
    {
      url: baseUrl,
      lastModified: new Date(),
    },
  ];

  for (const lang of languages) {
    const tutorials = await getTutorials(lang);
    urls.push({
      url: `${baseUrl}/${lang}`,
      lastModified: new Date(),
    });
    tutorials.forEach((tutorial) => {
      urls.push({
        url: `${baseUrl}/${lang}/${tutorial.slug}`,
        lastModified: new Date(),
      });
    });
  }

  return urls;
}