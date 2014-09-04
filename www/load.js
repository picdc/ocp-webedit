

var loadcontent = document.createElement("div");
loadcontent.innerHTML = "Loading ...";
loadcontent.id = "loading-msg";


function load(id) {
    var el = document.getElementById(id);
    el.appendChild(loadcontent);
    main(id);
}
