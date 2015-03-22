jest.dontMock('../home');
jest.dontMock('../../components/search');

import React from 'react/addons';
import Home from '../home';
import Actions from '../../actions/';

describe('Home', () => {
    it('allows searching for a song', () => {
        const TestUtils = React.addons.TestUtils,
              home = TestUtils.renderIntoDocument(
                  <Home />
              );

        const searchInput = TestUtils.scryRenderedDOMComponentsWithTag(home, 'input')[0],
              form  = TestUtils.findRenderedDOMComponentWithTag(home, 'form');

        Actions.searchSong = jest.genMockFunction();
        TestUtils.Simulate.change(searchInput, {target: {value: 'song search'}});
        TestUtils.Simulate.submit(form);
        expect(Actions.searchSong).toBeCalledWith('song search');
    });
});

