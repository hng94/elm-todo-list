import './bootstrap.css';
import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';

var key = 'hoang-todos';

var storedState = localStorage.getItem(key);
var startingState = storedState ? JSON.parse(storedState) : null;

var app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: startingState
});

app.ports.setStorage.subscribe(function(data) {
  localStorage.setItem(key, JSON.stringify(data));
});

registerServiceWorker();
