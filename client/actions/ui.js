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

}

export default new UIActions();
