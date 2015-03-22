jest.dontMock('../search');

import React from 'react/addons';
import Search from '../search';

describe('Search', () => {
    it('allows user to search for a song, artist or album', () => {
        const TestUtils = React.addons.TestUtils,
              onSearchMock = jest.genMockFunction(),
              search  = TestUtils.renderIntoDocument(
                  <Search onSearch={onSearchMock} />
              );

        const searchInput = TestUtils.scryRenderedDOMComponentsWithTag(search, 'input')[0],
              form  = TestUtils.findRenderedDOMComponentWithTag(search, 'form');

        TestUtils.Simulate.change(searchInput, {target: {value: 'test search'}});
        TestUtils.Simulate.submit(form);
        expect(onSearchMock).toBeCalledWith('test search');
    });
});
