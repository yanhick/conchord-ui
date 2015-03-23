var express = require('express');

var app = express();

app.get('/api/search', function (req, res) {
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

app.get('/api/songs/:id', function (req, res) {
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


app.use(express.static('./dist'));

app.listen(3000);
