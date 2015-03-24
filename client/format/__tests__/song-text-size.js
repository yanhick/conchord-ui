jest.dontMock('../song-text-size');

import formatSongTextSize from '../song-text-size';

describe('formatSongTextSize', () => {
    it('converts the relative font size to CSS', () => {
        expect(formatSongTextSize(100)).toBe('100%');
    });
});
