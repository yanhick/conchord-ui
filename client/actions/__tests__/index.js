jest.dontMock('../../dispatcher');
jest.dontMock('../../constants');
jest.dontMock('../');

import $ from 'jquery';

import Dispatcher from '../../dispatcher/';
import Constants from '../../constants/';
import Actions from '../';

describe('Actions', () => {
    describe('#getSong', () => {
        it('fetches the song with the provided id', () => {

            Actions.getSong(1);

            expect($.ajax).toBeCalledWith({
                type: 'GET',
                url: 'api/songs/1',
                success: jasmine.any(Function)
            });
        });
    });

    describe('#searchSong', () => {
        it('search for songs with matching names', () => {

            Actions.searchSong('my search query');

            expect($.ajax).toBeCalledWith({
                type: 'GET',
                url: 'api/search',
                data: {
                    q: 'my search query'
                },
                success: jasmine.any(Function)
            });
        });
    });
});

