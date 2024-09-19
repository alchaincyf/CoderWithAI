import React from 'react'
import Link from 'next/link'

export default function Contact() {
  return (
    <div className="container mx-auto px-4 py-8">
      <h1 className="text-3xl font-bold mb-4">Contact Us</h1>
      <p className="mb-4">
        We value your feedback and are always here to assist you. If you have any questions, suggestions, or concerns, please don't hesitate to reach out to us.
      </p>
      <h2 className="text-2xl font-semibold mb-2">Get in Touch</h2>
      <ul className="list-disc list-inside mb-4">
        <li>Email: support@programmingtutorials.com</li>
        <li>Phone: +1 (555) 123-4567</li>
        <li>Address: 123 Coding Street, Tech City, TC 12345</li>
      </ul>
      <h2 className="text-2xl font-semibold mb-2">Follow Us</h2>
      <p className="mb-4">
        Stay updated with our latest tutorials and programming tips by following us on social media:
      </p>
      <ul className="list-disc list-inside mb-4">
        <li>Twitter: @ProgrammingTuts</li>
        <li>Facebook: /ProgrammingTutorials</li>
        <li>LinkedIn: /company/programming-tutorials</li>
      </ul>
      <p className="mb-4">
        We aim to respond to all inquiries within 24-48 hours during business days.
      </p>
      <Link href="/" className="text-blue-500 hover:underline">
        Return to Home
      </Link>
    </div>
  )
}