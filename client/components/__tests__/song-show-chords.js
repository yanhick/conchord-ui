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

        const checkbox = TestUtils.findRenderedDOMComponentWithTag(songShowChords, 'input');

        expect(checkbox.getDOMNode().checked).toBe(true);

        TestUtils.Simulate.change(checkbox);
        expect(onToggleShowChordsMock).toBeCalled();
    });
});




