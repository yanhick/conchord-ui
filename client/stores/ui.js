import events from 'events';

import Dispatcher from '../dispatcher/';
import UIConstants from '../constants/ui';

const CHANGE_EVENT = 'change';

let songTextRelativeSize = 100,
    showDuplicatedChoruses = true;

function incrementFontSize() {
    songTextRelativeSize += 10;
    this.emit(CHANGE_EVENT);
}

function decrementFontSize() {
    songTextRelativeSize -= 10;
    this.emit(CHANGE_EVENT);
}

function setShowDuplicatedChoruses(value) {
    showDuplicatedChoruses = value;
    this.emit(CHANGE_EVENT);
}

class UIStore extends events.EventEmitter {

    getSongTextRelativeSize() {
        return songTextRelativeSize;
    }

    getShowDuplicatedChoruses() {
        return showDuplicatedChoruses;
    }
}

const uiStore = new UIStore();

Dispatcher.register(function (action) {
    switch(action.actionType) {

        case UIConstants.INCREMENT_FONT_SIZE:
            incrementFontSize.call(uiStore);
            break;

        case UIConstants.DECREMENT_FONT_SIZE:
            decrementFontSize.call(uiStore);
            break;

        case UIConstants.SHOW_DUPLICATED_CHORUSES:
            setShowDuplicatedChoruses.call(uiStore, true);
            break;

        case UIConstants.HIDE_DUPLICATED_CHORUSES:
            setShowDuplicatedChoruses.call(uiStore, false);
            break;

        case UIConstants.TOGGLE_SHOW_DUPLICATED_CHORUSES:
            setShowDuplicatedChoruses.call(uiStore, !showDuplicatedChoruses);
            break;

        default:
    }
});

export default uiStore;
