function light() {
  localStorage.setItem("theme", "light");
  document.getElementById("theme-link").href="./chapter-light.css"
}

function dark() {
  localStorage.setItem("theme", "dark");
  document.getElementById("theme-link").href="./chapter-dark.css"
}

def_theme = light;

function loadTheme() {
  var theme = localStorage.getItem("theme");
  if (theme === "light") {
    light();
  } else if (theme === "dark") {
    dark();
  } else {
    def_theme();
  }
}

loadTheme();
