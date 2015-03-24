import events from 'events';

import Dispatcher from '../dispatcher/';
import UIConstants from '../constants/ui';

const CHANGE_EVENT = 'change';

let songTextRelativeSize = 100;

function incrementFontSize() {
    songTextRelativeSize += 10;
    this.emit(CHANGE_EVENT);
}

function decrementFontSize() {
    songTextRelativeSize -= 10;
    this.emit(CHANGE_EVENT);
}

class UIStore extends events.EventEmitter {

    getSongTextRelativeSize() {
        return songTextRelativeSize;
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

        default:
    }
});

export default uiStore;
