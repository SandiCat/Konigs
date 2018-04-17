'use strict';
import Elm from "./elm/Main.elm";
import localStoragePorts from "elm-local-storage-ports";
import "./elm/Stylesheets.elm";

var runningElmModule = Elm.Main.fullscreen();
localStoragePorts.register(runningElmModule.ports);