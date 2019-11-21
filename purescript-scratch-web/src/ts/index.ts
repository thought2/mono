import Main from '../../output/Main';

if (module.hot) {
  module.hot.accept();
  module.hot.dispose(function() {
    console.log(1);
    document.querySelector('#app').innerHTML = '';
  });
}

console.log(module.hot);

Main.main();
