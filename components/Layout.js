import Header from './Header';

const Layout = ({ children }) => {
  return (
    <>
      <Header />
      <main>{children}</main>
      {/* Footer component can be added here if needed */}
    </>
  );
};

export default Layout;
