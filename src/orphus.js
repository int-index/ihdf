function getSelectionText() {
    var text = "";
    if (window.getSelection) {
        text = window.getSelection().toString();
    } else if (document.selection && document.selection.type != "Control") {
        text = document.selection.createRange().text;
    }
    return text;
}

document.addEventListener("keypress", function(e){
    if (e.ctrlKey && e.key === "Enter") {
        var sel = getSelectionText();
        alertify
          .defaultValue("")
          .prompt("Comment on " + "“" + sel + "“",
            function (comment, ev) {
              var msg = "“" + sel + "” — " + comment;
              ev.preventDefault();
              emailjs.send("gmail", "ih", {"message":msg})
              alertify.success(msg);

            }, function(ev) {
              ev.preventDefault();
              alertify.error("Comment canceled");
            });
    }
});
