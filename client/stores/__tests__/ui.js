jest.dontMock('../../dispatcher');
jest.dontMock('../../constants/ui');
jest.dontMock('../ui');

import Dispatcher from '../../dispatcher/';
import UIConstants from '../../constants/ui';
import UIStore from '../ui';

describe('UIStore', () => {
    describe('#getSongTextRelativeSize', () => {
        it('returns the relative size of the text', () => {
            expect(UIStore.getSongTextRelativeSize()).toEqual(100);

            Dispatcher.dispatch({
                actionType: UIConstants.INCREMENT_FONT_SIZE
            });

            expect(UIStore.getSongTextRelativeSize()).toEqual(110);

            Dispatcher.dispatch({
                actionType: UIConstants.DECREMENT_FONT_SIZE
            });

            expect(UIStore.getSongTextRelativeSize()).toEqual(100);
        });
    });
});

