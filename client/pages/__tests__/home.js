jest.dontMock('../home');
jest.dontMock('../../components/search');
jest.dontMock('object-assign');

import assign from 'object-assign';

import React from 'react';
import ReactAddons from 'react/addons';
import Home from '../home';

/* wrap the Home to provide a stubbed router */
const stubRouterContext = (props, stubs) => {
  return React.createClass({

    childContextTypes: {
      router: React.PropTypes.object
    },

    getChildContext () {
      return {
          router: assign({
              transitionTo () {},
          }, stubs)
      };
    },

    render () {
        return (
            <Home {...props} />
        );
    }

  });
};

describe('Home', () => {
    it('allows searching for a song', () => {
        const TestUtils = React.addons.TestUtils,
              transitionToStub = jest.genMockFunction(),
              WrappedHome = stubRouterContext({}, { transitionTo: transitionToStub }),
              home = TestUtils.renderIntoDocument(
                  <WrappedHome />
              );

        const searchInput = TestUtils.scryRenderedDOMComponentsWithTag(home, 'input')[0],
              form  = TestUtils.findRenderedDOMComponentWithTag(home, 'form');

        TestUtils.Simulate.change(searchInput, {target: {value: 'song search'}});
        TestUtils.Simulate.submit(form);

        expect(transitionToStub).toBeCalledWith('search', {query: 'song search'});
    });
});

