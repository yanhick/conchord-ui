import React from 'react';
import SongSection from './song-section';

export default React.createClass({

    render: function () {

         const songSections = this.props.data.map((section) => {
             return (
                 <SongSection key={section.id} data={section} />
             );
        });

        return (
            <ul>
                {songSections}
            </ul>
        );

    }

});


