import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';


var app = Elm.Main.init({
  node: document.getElementById('root')
});

/*
window.onload = function() {
  // Parse the DOT syntax into a graphlib object.
  var g = graphlibDot.parse(document.getElementById("dot").value);

  // Render the graphlib object using d3.
  var renderer = new dagreD3.Renderer();
  renderer.run(g, d3.select("svg g"));

  // Optional - resize the SVG element based on the contents.
  var svg = document.querySelector('#graphContainer');
  var bbox = svg.getBBox();
  svg.style.width = bbox.width + 20.0 + "px";
  svg.style.height = bbox.height + 40.0 + "px";
}; */

app.ports.sendDOT.subscribe(function(data){
  console.log(data);

  // Parse the DOT syntax into a graphlib object.
  var g = graphlibDot.parse(data);

  // Render the graphlib object using d3.
  var renderer = new dagreD3.Renderer();
  renderer.run(g, d3.select("svg g"));

  // Optional - resize the SVG element based on the contents.
  var svg = document.querySelector('#graphContainer');
  var bbox = svg.getBBox();
  svg.style.width = bbox.width + 20.0 + "px";
  svg.style.height = bbox.height + 40.0 + "px";
});

serviceWorker.unregister();
