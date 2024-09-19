# Programming Tutorials

This project is a web application that provides programming tutorials for various languages.

## Features

1. **Dynamic Tutorial Content**: The application now loads real tutorial content from Markdown files stored in the project.
2. **Multiple Languages**: Supports multiple programming languages, with tutorials organized by language.
3. **Responsive Design**: The layout adapts to different screen sizes.
4. **Dynamic Language Selection**: The language selection bar adjusts based on the available screen width.
5. **Footer with External Link**: Includes a footer with a link to BookAI.TOP and additional navigation links.

## Project Structure

- `/tutorials`: Contains subdirectories for each programming language, each containing Markdown files for individual tutorials.
- `/components`: React components used in the application.
- `/lib`: Utility functions for fetching and processing tutorial data.

## Usage

To view the tutorials:
1. Navigate to the home page.
2. Select a programming language from the top bar.
3. Choose a tutorial from the sidebar.
4. Read the tutorial content in the main area.

## Development

This project uses Next.js and React. To run it locally:

1. Clone the repository
2. Install dependencies with `npm install`
3. Run the development server with `npm run dev`
4. Open [http://localhost:3000](http://localhost:3000) in your browser

## Adding New Tutorials

To add a new tutorial:
1. Create a new Markdown file in the appropriate language directory under `/tutorials`.
2. Add front matter to the Markdown file with `title` and `category` fields.
3. Write your tutorial content in Markdown format.

## Future Improvements

- Implement a search functionality for tutorials
- Add user authentication for progress tracking
- Enhance the Markdown rendering with syntax highlighting for code blocks
