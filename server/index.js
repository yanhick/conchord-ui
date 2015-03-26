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
    res.sendFile('song.json', {root: 'server'});
});

server.use(express.static('./dist'));

server.get('*', function (req, res) {
    res.sendFile('index.html', {root: './dist'});
});

module.exports = server;
