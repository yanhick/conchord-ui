jest.dontMock('../song-show-duplicated-choruses');

import React from 'react/addons';

import SongShowDuplicatedChoruses from '../song-show-duplicated-choruses';

describe('SongShowDuplicatedChoruses', () => {
    it('allows toggling the display of duplicated choruses', () => {
        const TestUtils = React.addons.TestUtils,
              onToggleShowDuplicatedChorusesMock = jest.genMockFunction(),
              songShowDuplicatedChoruses = TestUtils.renderIntoDocument(
                  <SongShowDuplicatedChoruses
                      showDuplicatedChoruses={true}
                      onToggleShowDuplicatedChoruses={onToggleShowDuplicatedChorusesMock} />
              );

        const checkbox = TestUtils.findRenderedDOMComponentWithTag(songShowDuplicatedChoruses, 'input');

        expect(checkbox.getDOMNode().checked).toBe(true);

        TestUtils.Simulate.change(checkbox);
        expect(onToggleShowDuplicatedChorusesMock).toBeCalled();
    });
});



