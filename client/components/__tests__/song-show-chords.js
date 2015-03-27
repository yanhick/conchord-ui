jest.dontMock('../song-show-chords');

import React from 'react/addons';

import SongShowChords from '../song-show-chords';

describe('SongShowChords', () => {
    it('allows toggling the display of chords', () => {
        const TestUtils = React.addons.TestUtils,
              onToggleShowChordsMock = jest.genMockFunction(),
              songShowChords = TestUtils.renderIntoDocument(
                  <SongShowChords
                      showChords={true}
                      onToggleShowChords={onToggleShowChordsMock} />
              );

        const button = TestUtils.findRenderedDOMComponentWithTag(songShowChords, 'button');

        TestUtils.Simulate.click(button);
        expect(onToggleShowChordsMock).toBeCalled();
    });
});




