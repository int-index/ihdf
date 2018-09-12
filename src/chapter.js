
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

function showSolution(btn) {
  btn.parentElement.getElementsByClassName('solution-content')[0].
    classList.remove('solution-hidden');
  btn.innerHTML = "Hide solution";
  btn.onclick = function() { hideSolution(btn) };
}

function hideSolution(btn) {
  btn.parentElement.getElementsByClassName('solution-content')[0].
    classList.add('solution-hidden');
  btn.innerHTML = "Show solution";
  btn.onclick = function() { showSolution(btn) };
}

