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

    describe('#hideDuplicatedChoruses', () => {
        it('hides duplicated choruses', () => {

            UIActions.hideDuplicatedChoruses();

            expect(Dispatcher.dispatch).toBeCalledWith({
                actionType: UIConstants.HIDE_DUPLICATED_CHORUSES
            });
        });
    });

    describe('#showDuplicatedChoruses', () => {
        it('shows duplicated choruses', () => {

            UIActions.showDuplicatedChoruses();

            expect(Dispatcher.dispatch).toBeCalledWith({
                actionType: UIConstants.SHOW_DUPLICATED_CHORUSES
            });
        });
    });

    describe('#toggleShowDuplicatedChoruses', () => {
        it('toggles hiding/showing duplicated choruses', () => {

            UIActions.toggleShowDuplicatedChoruses();

            expect(Dispatcher.dispatch).toBeCalledWith({
                actionType: UIConstants.TOGGLE_SHOW_DUPLICATED_CHORUSES
            });
        });
    });

    describe('#toggleShowChords', () => {
        it('toggles hiding/showing the song chords', () => {

            UIActions.toggleShowChords();

            expect(Dispatcher.dispatch).toBeCalledWith({
                actionType: UIConstants.TOGGLE_SHOW_CHORDS
            });
        });
    });

});


