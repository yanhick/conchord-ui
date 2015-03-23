import events from 'events';

import Dispatcher from '../dispatcher/';
import Constants from '../constants/';

const CHANGE_EVENT = 'change';

let searchResults = [],
    song = {
        meta: {},
        content: []
    };

function updateSearchResults(data) {
    searchResults = data;
    this.emit(CHANGE_EVENT);
}

function updateSong(data) {
    song = data;
    this.emit(CHANGE_EVENT);
}

class Store extends events.EventEmitter {

    getSearchResults() {
        return searchResults;
    }

    getSong() {
        return song;
    }

}

const store = new Store();

Dispatcher.register(function (action) {
    switch(action.actionType) {

        case Constants.UPDATE_SEARCH_RESULT:
            updateSearchResults.call(store, action.data);
            break;

        case Constants.UPDATE_SONG:
            updateSong.call(store, action.data);
            break;

        default:
    }
});

export default store;
