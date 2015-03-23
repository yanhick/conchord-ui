jest.dontMock('../../components/song');
jest.dontMock('../song');

import React from 'react/addons';
import Song from '../song';
import Actions from '../../actions/';
import Store from '../../stores/';

/* wrap the Song to provide a stubbed router */
const stubRouterContext = () => {
  return React.createClass({

    childContextTypes: {
      router: React.PropTypes.object
    },

    getChildContext () {
      return {
          router: {
              getCurrentQuery () {
                  return {
                      id: 1
                  };
              }
          }
      };
    },

    render () {
        return (
            <Song />
        );
    }

  });
};

describe('Song', () => {
    it('fetches the song with the current query params', () => {

        Actions.getSong = jest.genMockFunction();
        Store.getSong = jest.genMockFunction();
        Store.getSong.mockReturnValueOnce({
            id: 1,
            meta: {},
            content: {}
        });

        const TestUtils = React.addons.TestUtils,
              WrappedSong = stubRouterContext(),
              searchResults = TestUtils.renderIntoDocument(
                  <WrappedSong />
              );

        expect(Actions.getSong).toBeCalledWith(1);
    });
});


