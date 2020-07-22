## Elmlord
<p align="center">
  <img src="https://github.com/flofe104/elmlord/blob/master/src/assets/images/general/logo.png" width="450">  
</p>

Elmlord is a turn based singleplayer strategy game, which is completely developed in the programming language Elm. This project was developed as part of the university course _Functional Frontend Development_ at the [University of Flensburg](https://hs-flensburg.de/).

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


## Credits

The tasks were divided among ourselves as follows:
* @flofe104 (Florian Khors): Map / Mapgenerator, AI-Bots, Pathfinder / Pathagent, Model-Structure, MapActions, Sounds
* @serquicky (Michael Frank): Battle-System, Templates / User-Interface, Ports, Buildings / Settlement, Events

Special thanks 


