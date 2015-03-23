import $ from 'jquery';

import Dispatcher from '../dispatcher/';
import Constants from '../constants/';

class Actions {

    getSong(id) {
        $.ajax({
            type: 'GET',
            url: `api/songs/${id}`,
            success: (data) => {
                Dispatcher.dispatch({
                    actionType: Constants.UPDATE_SONG,
                    data: data
                });
            }
        });
    }

    searchSong(query) {
        $.ajax({
            type: 'GET',
            url: `api/search`,
            data: {
                q: query
            },
            success: (data) => {
                Dispatcher.dispatch({
                    actionType: Constants.UPDATE_SEARCH_RESULT,
                    data: data
                });
            }
        });
    }

}

export default new Actions();
