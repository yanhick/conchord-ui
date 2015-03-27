import React from 'react';
import _ from 'lodash';

import Chord from './chord';

export default React.createClass({

    render: function () {

        const chordList = _.map(this.props.data, (chord) => {
            return (
                <Chord key={chord.id} data={chord} />
            );
        });

        let classString = '';

        if (!this.props.showChords) {
            classString += 'no-chords';
        }

        return (
            <ul className={classString}>
                {chordList}
            </ul>
        );
    }

});

