jest.dontMock('../song-line');
jest.dontMock('../song-section');
jest.dontMock('../song-content');

import React from 'react/addons';
import SongContent from '../song-content';

describe('SongContent', () => {
    it('displays all the sections of the song', () => {
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
                  <SongContent data={data} />
              );

        expect(songContent.getDOMNode().textContent).toBe('VerseAlyrics');
    });
});

