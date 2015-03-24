jest.dontMock('../song-line');
jest.dontMock('../song-section');
jest.dontMock('../song-content');
jest.dontMock('../../format/song-text-size');

import React from 'react/addons';
import SongContent from '../song-content';
import formatSongTextSize from '../../format/song-text-size';

describe('SongContent', () => {
    it('displays all the sections of the song', () => {
        const songContent = createSongContent();

        expect(songContent.getDOMNode().textContent).toBe('VerseAlyrics');
    });

    it('applies the font size to the song content', () => {
        const songContent = createSongContent(123);

        expect(songContent.getDOMNode().style.fontSize).toBe('123%');
    });
});

function createSongContent(fontSize = 100) {
    const TestUtils = React.addons.TestUtils,
          data = [{
              id: 1,
              section: 'Verse',
              lines: [{
                  id: 1,
                  chordName: 'A',
                  content: 'lyrics'
              }]
          }],
          songContent = TestUtils.renderIntoDocument(
              <SongContent data={data} fontSize={fontSize}/>
          );

    return songContent;
}
