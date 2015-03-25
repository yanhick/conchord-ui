import Dispatcher from '../dispatcher/';
import UIConstants from '../constants/ui';

class UIActions {

    incrementFontSize() {
        Dispatcher.dispatch({
            actionType: UIConstants.INCREMENT_FONT_SIZE,
        });
    }

    decrementFontSize() {
        Dispatcher.dispatch({
            actionType: UIConstants.DECREMENT_FONT_SIZE,
        });
    }

    hideDuplicatedChoruses() {
        Dispatcher.dispatch({
            actionType: UIConstants.HIDE_DUPLICATED_CHORUSES,
        });
    }

    showDuplicatedChoruses() {
        Dispatcher.dispatch({
            actionType: UIConstants.SHOW_DUPLICATED_CHORUSES,
        });
    }

    toggleShowDuplicatedChoruses() {
        Dispatcher.dispatch({
            actionType: UIConstants.TOGGLE_SHOW_DUPLICATED_CHORUSES
        });
    }

    toggleShowChords() {
        Dispatcher.dispatch({
            actionType: UIConstants.TOGGLE_SHOW_CHORDS
        });
    }

}

export default new UIActions();
