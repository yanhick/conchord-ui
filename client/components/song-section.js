import React from 'react';
import SongLine from './song-line';

export default React.createClass({

    render: function () {

         const songLines = this.props.data.lines.map((line) => {
             return (
                 <SongLine key={line.id} data={line} />
             );
        });

        return (
            <section className={this.props.data.section}>
                {this.props.data.section}
                <ul>
                    {songLines}
                </ul>
            </section>
        );

    }

});

