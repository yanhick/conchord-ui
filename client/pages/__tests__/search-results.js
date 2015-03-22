jest.dontMock('../../components/results');
jest.dontMock('../../components/result');
jest.dontMock('../search-results');

import React from 'react/addons';
import SearchResults from '../search-results';
import Actions from '../../actions/';
import Store from '../../stores/';

describe('SearchResults', () => {
    it('allows selecting a song among results', () => {

        Store.getSearchResults = jest.genMockFunction();
        Store.getSearchResults.mockReturnValueOnce([{
            id: 1,
            title: 'my song',
            href: '/mysong'
        }]);
        Actions.getSong = jest.genMockFunction();

        const TestUtils = React.addons.TestUtils,
              searchResults = TestUtils.renderIntoDocument(
                  <SearchResults />
              );

        const link = TestUtils.findRenderedDOMComponentWithTag(searchResults, 'a');
        TestUtils.Simulate.click(link);

        expect(Actions.getSong).toBeCalledWith(1);
    });
});

