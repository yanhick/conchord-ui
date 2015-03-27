jest.dontMock('../push-down-button');

import React from 'react/addons';

import PushDownButton from '../push-down-button';

describe('PushDownButton', () => {
    it('allows toggling a value when clicked', () => {
        const TestUtils = React.addons.TestUtils,
              onClickMock = jest.genMockFunction(),
              pushDownButton = TestUtils.renderIntoDocument(
                  <PushDownButton
                      active={true}
                      icon="test"
                      onClick={onClickMock} />
              );

        const button = TestUtils.findRenderedDOMComponentWithTag(pushDownButton, 'button');

        expect(button.getDOMNode().className.indexOf('test')).toNotBe(-1);

        TestUtils.Simulate.click(button);
        expect(onClickMock).toBeCalled();
    });
});



