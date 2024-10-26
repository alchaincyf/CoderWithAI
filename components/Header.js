import Link from 'next/link';
import { useRouter } from 'next/router';

const Header = () => {
  const router = useRouter();

  const languages = [
    { name: 'JavaScript', path: '/JavaScript' },
    { name: 'Python', path: '/Python' },
    { name: 'Java', path: '/Java' },
    { name: 'C++', path: '/Cpp' },
    { name: 'PHP', path: '/PHP' },
    { name: 'C#', path: '/CSharp' }
  ];

  return (
    <header>
      <nav className="navbar navbar-expand-lg navbar-light bg-light">
        <div className="container-fluid">
          <Link href="/" className="navbar-brand">
            CoderWithAI
          </Link>
          <button className="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarNav" aria-controls="navbarNav" aria-expanded="false" aria-label="Toggle navigation">
            <span className="navbar-toggler-icon"></span>
          </button>
          <div className="collapse navbar-collapse" id="navbarNav">
            <ul className="navbar-nav me-auto">
              <li className="nav-item">
                <Link href="/" className={`nav-link ${router.pathname === '/' ? 'active' : ''}`}>
                  Home
                </Link>
              </li>
              {languages.map((lang) => (
                <li key={lang.path} className="nav-item">
                  <Link href={lang.path} className={`nav-link ${router.pathname.startsWith(lang.path) ? 'active' : ''}`}>
                    {lang.name}
                  </Link>
                </li>
              ))}
            </ul>
            <a href="https://github.com/alchaincyf/CoderWithAI" className="btn btn-outline-dark" target="_blank" rel="noopener noreferrer">
              <i className="bi bi-github"></i> GitHub
            </a>
          </div>
        </div>
      </nav>
    </header>
  );
};

export default Header;
