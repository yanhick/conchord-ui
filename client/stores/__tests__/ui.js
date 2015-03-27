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

    describe('#getShowDuplicatedChoruses', () => {
        it('returns whether duplicated choruses should be shown', () => {
            expect(UIStore.getShowDuplicatedChoruses()).toBe(true);

            Dispatcher.dispatch({
                actionType: UIConstants.HIDE_DUPLICATED_CHORUSES
            });

            expect(UIStore.getShowDuplicatedChoruses()).toBe(false);

            Dispatcher.dispatch({
                actionType: UIConstants.SHOW_DUPLICATED_CHORUSES
            });

            expect(UIStore.getShowDuplicatedChoruses()).toBe(true);

            Dispatcher.dispatch({
                actionType: UIConstants.TOGGLE_SHOW_DUPLICATED_CHORUSES
            });

            expect(UIStore.getShowDuplicatedChoruses()).toBe(false);
        });
    });

    describe('#getShowChords', () => {
        it('returns whether chords should be shown', () => {
            expect(UIStore.getShowChords()).toBe(true);

            Dispatcher.dispatch({
                actionType: UIConstants.TOGGLE_SHOW_CHORDS
            });

            expect(UIStore.getShowDuplicatedChoruses()).toBe(false);

        });
    });
});

