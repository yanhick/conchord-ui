import React from 'react';

import SongMeta from './song-meta';
import SongContent from './song-content';

export default React.createClass({

    render: function () {
        return (
            <main>
                <SongMeta data={this.props.data.meta} />
                <SongContent data={this.props.data.content} />
            </main>
        );
    }

});
