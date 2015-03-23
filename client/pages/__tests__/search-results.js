jest.dontMock('../../components/results');
jest.dontMock('../../components/result');
jest.dontMock('../search-results');
jest.dontMock('object-assign');

import assign from 'object-assign';

import React from 'react/addons';
import SearchResults from '../search-results';
import Actions from '../../actions/';
import Store from '../../stores/';

/* wrap the SearchResults to provide a stubbed router */
const stubRouterContext = (stubs) => {
  return React.createClass({

    childContextTypes: {
      router: React.PropTypes.object
    },

    getChildContext () {
      return {
          router: assign({
              getCurrentQuery () {
                  return {
                      q: 'search query'
                  };
              }
          }, stubs)
      };
    },

    render () {
        return (
            <SearchResults />
        );
    }

  });
};

describe('SearchResults', () => {
    beforeEach(() => {
        Store.getSearchResults = jest.genMockFunction();
        Store.getSearchResults.mockReturnValueOnce([{
            id: 1,
            title: 'my song',
            href: '/mysong'
        }]);
    });

    it('fetches the songs with the current query params', () => {

        Actions.searchSong = jest.genMockFunction();

        const TestUtils = React.addons.TestUtils,
              WrappedSearchResults = stubRouterContext(),
              searchResults = TestUtils.renderIntoDocument(
                  <WrappedSearchResults />
              );

        expect(Actions.searchSong).toBeCalledWith('search query');
    });

    it('allows selecting a song among results', () => {

        const TestUtils = React.addons.TestUtils,
              transitionToStub = jest.genMockFunction(),
              WrappedSearchResults = stubRouterContext({transitionTo: transitionToStub}),
              searchResults = TestUtils.renderIntoDocument(
                  <WrappedSearchResults />
              );

        const link = TestUtils.findRenderedDOMComponentWithTag(searchResults, 'a');
        TestUtils.Simulate.click(link);

        expect(transitionToStub).toBeCalledWith('song', {}, {id: 1});
    });
});
