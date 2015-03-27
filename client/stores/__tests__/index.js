jest.dontMock('../../dispatcher');
jest.dontMock('../../constants');
jest.dontMock('../');

import Dispatcher from '../../dispatcher/';
import Constants from '../../constants/';
import Store from '../';

describe('Store', () => {
    describe('#getSearchResults', () => {
        it('returns the latest search results', () => {
            expect(Store.getSearchResults()).toEqual([]);

            Dispatcher.dispatch({
                actionType: Constants.UPDATE_SEARCH_RESULT,
                data: [{foo: 'bar'}]
            });

            expect(Store.getSearchResults()).toEqual([{foo: 'bar'}]);
        });
    });

    describe('#getSong', () => {
        it('returns the song data', () => {
            expect(Store.getSong()).toEqual({
                chords: {},
                meta: {},
                content: []
            });

            Dispatcher.dispatch({
                actionType: Constants.UPDATE_SONG,
                data: {foo: 'bar'}
            });

            expect(Store.getSong()).toEqual({foo: 'bar'});
        });
    });
});
