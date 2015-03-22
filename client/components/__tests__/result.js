jest.dontMock('../result');

import React from 'react/addons';
import Result from '../result';

describe('Result', () => {
    it('displays a selectable search item result', () => {
        const TestUtils = React.addons.TestUtils,
              onSelectMock = jest.genMockFunction(),
              data = {
                  id: 1,
                  title: 'my song',
                  href: '/mysong'
              },
              result = TestUtils.renderIntoDocument(
                  <Result data={data} onSelect={onSelectMock} />
              );

        const songLink = TestUtils.findRenderedDOMComponentWithTag(result, 'a');

        TestUtils.Simulate.click(songLink);
        expect(onSelectMock).toBeCalledWith(1);
    });
});

