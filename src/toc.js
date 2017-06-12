function light() {
  document.getElementById("theme-link").href="./toc-light.css"
}

function dark() {
  document.getElementById("theme-link").href="./toc-dark.css"
}

def_theme = light;

function loadTheme() {
  var theme = sessionStorage.getItem("theme");
  if (theme === "light") {
    light();
  } else if (theme === "dark") {
    dark();
  } else {
    def_theme();
  }
}

loadTheme();
