var theme = localStorage.getItem("theme");
if (theme === "light") {
  document.getElementById("theme-link").href="./toc-light.css"
} else if (theme === "dark") {
  document.getElementById("theme-link").href="./toc-dark.css"
}
