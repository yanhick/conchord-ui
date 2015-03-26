jest.dontMock('../song-text-size');

import React from 'react/addons';
import Result from '../result';

import SongTextSize from '../song-text-size';

describe('SongTextSize', () => {
    it('allows to increment the font size', () => {
        const TestUtils = React.addons.TestUtils,
              onIncrementMock = jest.genMockFunction(),
              songTextSize = TestUtils.renderIntoDocument(
                   <SongTextSize onIncrement={onIncrementMock} />
              );

        const incrementButton = TestUtils.scryRenderedDOMComponentsWithTag(songTextSize, 'button')[1];

        TestUtils.Simulate.click(incrementButton);
        expect(onIncrementMock).toBeCalled();
    });

    it('allows to decrement the font size', () => {
        const TestUtils = React.addons.TestUtils,
              onDecrementMock = jest.genMockFunction(),
              songTextSize = TestUtils.renderIntoDocument(
                   <SongTextSize onDecrement={onDecrementMock} />
              );

        const decrementButton = TestUtils.scryRenderedDOMComponentsWithTag(songTextSize, 'button')[0];

        TestUtils.Simulate.click(decrementButton);
        expect(onDecrementMock).toBeCalled();
    });
});


