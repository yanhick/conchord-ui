jest.dontMock('../song-line');

import React from 'react/addons';
import SongLine from '../song-line';

describe('SongLine', () => {
    it('displays one line of the song', () => {
        const TestUtils = React.addons.TestUtils,
              data = {
                  chordName: 'A',
                  content: 'lyrics'
              },
              songLine = TestUtils.renderIntoDocument(
                  <SongLine data={data} />
              );

        expect(songLine.getDOMNode().textContent).toBe('Alyrics');
    });
});

