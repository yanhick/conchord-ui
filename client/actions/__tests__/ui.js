jest.dontMock('../../constants/ui');
jest.dontMock('../ui');

import Dispatcher from '../../dispatcher/';
import UIConstants from '../../constants/ui';
import UIActions from '../ui';

describe('UIActions', () => {
    describe('#incrementFontSize', () => {
        it('increments the song text size', () => {

            UIActions.incrementFontSize();

            expect(Dispatcher.dispatch).toBeCalledWith({
                actionType: UIConstants.INCREMENT_FONT_SIZE
            });
        });
    });

    describe('#decrementFontSize', () => {
        it('decrements the song text size', () => {

            UIActions.decrementFontSize();

            expect(Dispatcher.dispatch).toBeCalledWith({
                actionType: UIConstants.DECREMENT_FONT_SIZE
            });
        });
    });

});


