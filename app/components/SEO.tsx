import Head from 'next/head';

interface SEOProps {
  title: string;
  description: string;
  keywords?: string;
  ogImage?: string;
  canonicalUrl?: string;
  datePublished?: string;
  category?: string;
}

export default function SEO({ 
  title, 
  description, 
  keywords, 
  ogImage, 
  canonicalUrl, 
  datePublished,
  category
}: SEOProps) {
  const structuredData = {
    "@context": "https://schema.org",
    "@type": "Article",
    "headline": title,
    "description": description,
    "image": ogImage,
    "datePublished": datePublished,
    "author": {
      "@type": "Organization",
      "name": "CoderWithAI"
    },
    "publisher": {
      "@type": "Organization",
      "name": "CoderWithAI",
      "logo": {
        "@type": "ImageObject",
        "url": "https://www.coderwith.ai/logo.png"
      }
    },
    "mainEntityOfPage": {
      "@type": "WebPage",
      "@id": canonicalUrl
    },
    "articleSection": category
  };

  return (
    <Head>
      <title>{title}</title>
      <meta name="description" content={description} />
      {keywords && <meta name="keywords" content={keywords} />}
      <meta property="og:title" content={title} />
      <meta property="og:description" content={description} />
      {ogImage && <meta property="og:image" content={ogImage} />}
      <meta name="twitter:card" content="summary_large_image" />
      {canonicalUrl && <link rel="canonical" href={canonicalUrl} />}
      {datePublished && <meta property="article:published_time" content={datePublished} />}
      {category && <meta property="article:section" content={category} />}
      <script type="application/ld+json">
        {JSON.stringify(structuredData)}
      </script>
    </Head>
  );
}