jest.dontMock('../song-line');
jest.dontMock('../song-section');

import React from 'react/addons';
import SongSection from '../song-section';

describe('SongSection', () => {
    it('displays one section of the song', () => {
        const TestUtils = React.addons.TestUtils,
              data = {
                  section: 'Verse',
                  lines: [{
                      id: 1,
                      chordName: 'A',
                      content: 'lyrics'
                  }]
              },
              songSection = TestUtils.renderIntoDocument(
                  <SongSection data={data} />
              );

        expect(songSection.getDOMNode().textContent).toBe('VerseAlyrics');
    });
});

