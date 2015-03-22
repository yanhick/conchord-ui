jest.dontMock('../results');
jest.dontMock('../result');

import React from 'react/addons';
import Results from '../results';

describe('Results', () => {
    it('displays a list of selectable search item results', () => {
        const TestUtils = React.addons.TestUtils,
              onSelectMock = jest.genMockFunction(),
              data = [{
                  id: 1,
                  title: 'my song',
                  href: '/mysong'
              }, {
                  id: 2,
                  title: 'my other song',
                  href: '/my-other-song'

              }],
              results = TestUtils.renderIntoDocument(
                  <Results data={data} onSelect={onSelectMock} />
              );

        const songLinks = TestUtils.scryRenderedDOMComponentsWithTag(results, 'a');

        TestUtils.Simulate.click(songLinks[1]);
        expect(onSelectMock).toBeCalledWith(2);
    });
});

