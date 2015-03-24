jest.dontMock('../../components/song');
jest.dontMock('../song');

import React from 'react/addons';
import Song from '../song';
import Actions from '../../actions/';
import Store from '../../stores/';
import UIStore from '../../stores/ui';

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

    beforeEach(() => {
        Actions.getSong = jest.genMockFunction();

        Store.getSong = jest.genMockFunction();
        Store.getSong.mockReturnValueOnce({
            id: 1,
            meta: {},
            content: {}
        });

        UIStore.getSongTextRelativeSize = jest.genMockFunction();
        UIStore.getSongTextRelativeSize.mockReturnValueOnce(123);
    });

    it('fetches the song with the current query params', () => {

        const TestUtils = React.addons.TestUtils,
              WrappedSong = stubRouterContext(),
              song = TestUtils.renderIntoDocument(
                  <WrappedSong />
              );

        expect(Actions.getSong).toBeCalledWith(1);
    });

    it('gets the size of the song text', () => {

        const TestUtils = React.addons.TestUtils,
              WrappedSong = stubRouterContext(),
              song = TestUtils.renderIntoDocument(
                  <WrappedSong />
              );
    });
});


