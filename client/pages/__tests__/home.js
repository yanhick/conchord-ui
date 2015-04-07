jest.dontMock('../home');
jest.dontMock('../../components/search');
jest.dontMock('object-assign');

import assign from 'object-assign';

import React from 'react';
import ReactAddons from 'react/addons';
import Home from '../home';

/* stub the router with just what's needed */
const routerStub = {
    router: {
        transitionTo: jest.genMockFunction()
    }
};

/* wrap the Home to provide a stubbed router */
const stubRouterContext = () => {
  return React.createClass({

    childContextTypes: {
      router: React.PropTypes.object
    },

    getChildContext () {
      return routerStub;
    },

    render () {
        return (
            <Home />
        );
    }

  });
};

describe('Home', () => {
    it('allows searching for a song', () => {
        const TestUtils = React.addons.TestUtils,
              transitionToStub = jest.genMockFunction(),
              WrappedHome = stubRouterContext(),
              home = TestUtils.renderIntoDocument(
                  <WrappedHome />
              );

        const searchInput = TestUtils.scryRenderedDOMComponentsWithTag(home, 'input')[0],
              button = TestUtils.findRenderedDOMComponentWithTag(home, 'button');

        TestUtils.Simulate.change(searchInput, {target: {value: 'song search'}});
        TestUtils.Simulate.click(button);

        expect(routerStub.router.transitionTo).toBeCalledWith('search', {}, {q: 'song search'});
    });
});

