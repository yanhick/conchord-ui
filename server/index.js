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
    res.send(createStubSong());
});

/* create stub song */

function createStubSong () {
    var sections = [
        'Intro',
        'Verse1',
        'Chorus',
        'Verse2',
        'Chorus',
        'Bridge',
        'Chorus'
    ];

    var buildSection = createSectionBuilder();
    var content = sections.map(buildSection);

    return {
        id: 1,
        chords: createStubChords(),
        meta: {
            title: 'my-title',
            artist: 'my artist',
            album: 'my album',
        },
        content: content
    };
}

function createStubChords () {
    return {
        'G': {
            id: 1,
            notes: {
                'e': 3,
                'B': 3,
                'G': '-',
                'D': '-',
                'A': 2,
                'E': 3
            }
        },
        'C': {
            id: 2,
            notes: {
                'e': 5,
                'B': 2,
                'G': '-',
                'D': '-',
                'A': 3,
                'E': 1
            }
        },
        'F': {
            id: 3,
            notes: {
                'e': 4,
                'B': 1,
                'G': '-',
                'D': '-',
                'A': 3,
                'E': 2
            }
        }
    };
}

function createSectionBuilder() {
    var id = 0;
    return function (name) {
        var createLines = createLinesBuilder();
        return {
            id: ++id,
            section: name,
            lines: createLines()
        };
    };
}

function createLinesBuilder() {
    var id = 0;
    return function () {
        var lines = [];
        for (var i = 0; i < 6; i++) {
            lines.push({
                id: ++id,
                chordName: 'A',
                content: createLine()
            });
        }
        return lines;
    };
}

function createLine () {
    var length = Math.ceil(Math.random() * 15),
        words = ['List', 'of', 'random', 'song', 'words', 'with', 'different', 'lengths'],
        line = [];

        for (var i = 0; i < length; i++) {
            line.push(words[Math.floor(Math.random() * words.length)]);
        }

        return line.join(' ');
}

server.use(express.static('./dist'));

server.get('*', function (req, res) {
    res.sendFile('index.html', {root: './dist'});
});

module.exports = server;
