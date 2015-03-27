jest.dontMock('../chords');

import React from 'react/addons';
import Chords from '../chords';

describe('Chords', () => {
    it('adds a class to hide chords', () => {
        const TestUtils = React.addons.TestUtils,
              data = {
                  'A': {
                      id: 1,
                      notes: {}
                  }
              },
              chords = TestUtils.renderIntoDocument(
                  <Chords data={data}
                          showChords={false}/>
              );

        expect(chords.getDOMNode().className).toBe('no-chords');
    });
});
