import React from 'react';

import SongSection from './song-section';
import formatSongTextSize from '../format/song-text-size';

export default React.createClass({

    render: function () {

        const songSections = this.props.data.map((section) => {
             return (
                 <SongSection key={section.id} data={section} />
             );
        });

        const styles = {
            fontSize: formatSongTextSize(this.props.fontSize)
        };

        return (
            <ul style={styles}>
                {songSections}
            </ul>
        );

    }

});


