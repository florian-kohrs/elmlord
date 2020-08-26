## Elmlord
<p align="center">
  <img src="https://github.com/flofe104/elmlord/blob/master/src/assets/images/general/logo.png" width="450">  
</p>

Elmlord is a turn based singleplayer strategy game, which is completely developed in the programming language Elm. This project was developed as part of the university course [Functional Frontend Development](https://jan-christiansen.github.io/page/teaching/2020/functional-frontend-development.html)  at the [University of Flensburg](https://hs-flensburg.de/).

## Documentation
Under the tab _Wiki_ is an extensive game manual for the game, its is highly suggested that new players read this before their first game.

## Building

#### Development
During the development the [interactive development tool](https://github.com/elm-lang/elm-reactor) should be used.

```
elm reactor
```

#### Rollout
When the development is finished, the application needs to be recompiled to JavaScript.

```
elm make src/Main.elm --output elm.js
```

After this you just have to open the _index.html_ in a browser.

The application is currently hosted under a [netlify site](https://kind-ardinghelli-25b6c3.netlify.app/), we still recommend to build and run it locally, because of the assets. The netlify site lags a bit because of the assets (images, sounds, etc.).

## Credits

The tasks were divided among ourselves as follows:
* [@flofe104 (Florian Kohrs):](https://github.com/flofe104) Map / Mapgenerator, AI-Bots, Pathfinder / Pathagent, Model-Structure, MapActions, Music composition/ Guitar recording (Clarinette and Low Wistle by Felix Kohrs)
* [@serquicky (Michael Frank):](https://github.com/serquicky) Battle-System, Templates / User-Interface, Ports, Buildings / Settlement, Events

Special thanks to [MDomroese](https://gitlab.com/MDomroese) for the settlement illustration and the great lord profile pictures.

Background-Style: [old paper or parchment - The Misguided Millennial](https://www.pinterest.de/pin/817333032353382769/)
