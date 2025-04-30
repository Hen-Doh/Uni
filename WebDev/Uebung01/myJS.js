document.addEventListener("DOMContentLoaded", function () {
    const changeButton = document.getElementById("newText");
    const textField = document.getElementById("textField");
  
    changeButton.addEventListener("click", function () {
      textField.value = "TODO";
    });
  });