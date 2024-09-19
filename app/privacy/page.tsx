import React from 'react'
import Link from 'next/link'

export default function Privacy() {
  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-4">Privacy Policy</h1>
      <p className="mb-4">
        At CodeWithAI, we are committed to protecting your privacy and ensuring the security of your personal information. This Privacy Policy outlines how we collect, use, and safeguard your data when you use our website.
      </p>
      <h2 className="text-2xl font-semibold mb-2">Information We Collect</h2>
      <p className="mb-4">
        We may collect the following types of information:
      </p>
      <ul className="list-disc list-inside mb-4">
        <li>Personal information (e.g., name, email address) when you voluntarily provide it</li>
        <li>Usage data (e.g., pages visited, time spent on the site)</li>
        <li>Technical data (e.g., IP address, browser type)</li>
      </ul>
      <h2 className="text-2xl font-semibold mb-2">How We Use Your Information</h2>
      <p className="mb-4">
        We use the collected information to:
      </p>
      <ul className="list-disc list-inside mb-4">
        <li>Provide and improve our services</li>
        <li>Personalize your experience</li>
        <li>Communicate with you about our services</li>
        <li>Analyze and monitor usage of our website</li>
      </ul>
      <h2 className="text-2xl font-semibold mb-2">Data Security</h2>
      <p className="mb-4">
        We implement appropriate technical and organizational measures to protect your personal information against unauthorized access, alteration, disclosure, or destruction.
      </p>
      <h2 className="text-2xl font-semibold mb-2">Your Rights</h2>
      <p className="mb-4">
        You have the right to access, correct, or delete your personal information. If you have any questions or requests regarding your data, please contact us.
      </p>
      <p className="mb-4">
        This privacy policy is subject to change. We encourage you to review it periodically.
      </p>
      <p className="mb-4">
        Last updated: {new Date().toLocaleDateString()}
      </p>
      <Link href="/" className="text-blue-500 hover:underline">
        Return to Home
      </Link>
    </div>
  )
}