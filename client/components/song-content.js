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

        let classString = '';

        if (!this.props.showDuplicatedChoruses) {
            classString += 'no-duplicated-choruses';
        }

        return (
            <article>
                <ul style={styles} className={classString}>
                    {songSections}
                </ul>
            </article>
        );

    }

});


