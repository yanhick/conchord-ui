'use strict';

exports.mkSongFullscreen = function () {
    var song = document.getElementsByTagName('main')[0];
    if (song) {
        if (song.requestFullscreen) {
            song.requestFullscreen();
        }
        else if (song.mozRequestFullScreen) {
            song.mozRequestFullScreen();
        }
        else if (song.webkitRequestFullScreen) {
            song.webkitRequestFullScreen();
        }
    }
}
