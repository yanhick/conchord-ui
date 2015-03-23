var express = require('express');

var server = express();

server.get('/api/search', function (req, res) {
    res.set('Content-Type', 'application/json');
    res.send([{
        id: 1,
        title: 'my song',
        href: '/my-song'
    }, {
        id: 2,
        title: 'my other song',
        href: '/my-other-song'
    }]);
});

server.get('/api/songs/:id', function (req, res) {
    res.set('Content-Type', 'application/json');
    res.send({
        id: 1,
        title: 'my-title',
        artist: 'my artist',
        album: 'my album',
        content: [{
            id: 1,
            section: 'Verse',
            lines: [{
                id: 1,
                chordName: 'A',
                content: 'my lyrics'
            }]
        }]
    });
});

server.use(express.static('./dist'));

module.exports = server;
